use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
    path::{Path, PathBuf},
    time::SystemTime,
};

use egui::Modal;

#[derive(Clone, Debug, Default)]
pub enum Contents {
    #[default]
    Unread,
    Unreadable,
    Directory,
    String(String),
    Bytes(Vec<u8>),
}
impl Contents {
    pub fn new(bytes: Vec<u8>) -> Self {
        match String::from_utf8(bytes) {
            Ok(str) => Self::String(str),
            Err(err) => Self::Bytes(err.into_bytes()),
        }
    }

    pub fn as_str(&mut self) -> Option<&str> {
        match self {
            Self::String(str) => Some(str),
            _ => None,
        }
    }

    pub fn as_string_mut(&mut self) -> Option<&mut String> {
        match self {
            Self::String(str) => Some(str),
            _ => None,
        }
    }

    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            Self::Bytes(items) => Some(items),
            Self::String(str) => Some(str.as_bytes()),
            Self::Unread => None,
            Self::Unreadable => None,
            Self::Directory => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FileOwned {
    pub contents: Contents,
    pub last_modified: SystemTime,
}

#[derive(Debug, Default)]
pub struct ProjectFiles {
    project: Option<PathBuf>,
    files: BTreeMap<PathBuf, FileOwned>,
    dirty: BTreeSet<PathBuf>,
    errors: BTreeMap<PathBuf, ErrorKind>,
}

#[derive(Debug)]
pub enum ErrorKind {
    FileContentsNewer,
    CannotOpenFile,
    CannotSaveFile,
}

impl ProjectFiles {
    pub fn new(project: Option<PathBuf>) -> Self {
        Self {
            project,
            ..Default::default()
        }
    }
    pub fn project_path(&self) -> Option<&Path> {
        self.project.as_deref()
    }

    pub fn set_project_path(&mut self, path: impl Into<PathBuf>) {
        self.project = Some(path.into());
    }

    pub fn close_project(&mut self) {
        self.project = None;
        self.files.clear();
    }

    pub fn is_dirty(&self) -> bool {
        !self.dirty.is_empty()
    }

    pub fn is_path_dirty(&self, path: impl AsRef<Path>) -> bool {
        self.dirty.contains(path.as_ref())
    }

    pub fn create_file(&mut self, path: impl Into<PathBuf>) {}

    pub fn create_dir(&mut self, path: impl Into<PathBuf>) {}

    pub fn files(&self) -> impl Iterator<Item = (&PathBuf, &FileOwned)> {
        self.files.iter()
    }

    pub fn error_ui(&mut self, ctx: &egui::Context) {
        self.errors.retain(|path, error| {
            Modal::new(format!("error {}", path.display()).into())
                .show(ctx, |ui| match error {
                    ErrorKind::FileContentsNewer => {
                        ui.label("File contents differ");
                        if ui.button("Overrite").clicked() {
                            ui.close();
                            return false;
                        }
                        if ui.button("Reload").clicked() {
                            ui.close();
                            return false;
                        }
                        true
                    }
                    ErrorKind::CannotOpenFile => {
                        ui.label("Cannot open file");
                        if ui.button("Discard").clicked() {
                            ui.close();
                            return false;
                        }
                        if ui.button("Create file").clicked() {
                            ui.close();
                            return false;
                        }
                        true
                    }
                    ErrorKind::CannotSaveFile => false,
                })
                .inner
        });
    }

    pub fn sync(&mut self) {
        let Some(project) = self.project.clone() else {
            return;
        };
        for path in &self.dirty {
            if self.errors.contains_key(path){
                continue;
            }
            let Some(cached) = self.files.get(path) else {
                continue;
            };
            let real_path = project.join(path);
            
            let contents = match &cached.contents {
                Contents::String(bytes) => bytes.as_bytes(),
                Contents::Bytes(items) => items,
                _ => continue,
            };
            let real_time = real_path
                .metadata()
                .ok()
                .and_then(|v| v.modified().ok())
                .unwrap_or(SystemTime::UNIX_EPOCH);
            let cached_time = cached.last_modified;
            if real_time > cached_time {
                self.errors.insert(path.to_path_buf(), ErrorKind::FileContentsNewer);
                continue;
            }
            let Ok(mut file) = std::fs::OpenOptions::new().write(true).read(true).open(&real_path) else {
                self.errors.insert(path.to_path_buf(), ErrorKind::CannotOpenFile);
                continue;
            };
            if file.write_all(contents).is_err() {
                self.errors.insert(path.to_path_buf(), ErrorKind::CannotSaveFile);
                continue;
            }
            if file.set_len(contents.len() as u64).is_err(){
                self.errors.insert(path.to_path_buf(), ErrorKind::CannotSaveFile);
                continue;
            }
        }
        self.dirty.clear();
        self.explore(project);
    }

    fn explore(&mut self, path: PathBuf) {
        let Some(project) = &self.project else {
            return;
        };
        let Ok(meta) = path.metadata() else {
            return;
        };
        let Ok(stripped) = path.strip_prefix(project) else{
            return;
        };
        if meta.is_dir() {
            self.files.insert(
                stripped.to_path_buf(),
                FileOwned {
                    contents: Contents::Directory,
                    last_modified: meta.modified().unwrap_or(SystemTime::now()),
                },
            );
            for entry in path.read_dir().into_iter().flatten().flatten() {
                self.explore(entry.path());
            }
        } else if path.is_file() {
            if self.errors.contains_key(stripped) {
                return;
            }
            if meta.modified().unwrap_or(SystemTime::UNIX_EPOCH)
                <= self
                    .files
                    .get(stripped)
                    .map(|v| v.last_modified)
                    .unwrap_or(SystemTime::UNIX_EPOCH)
            {
                return;
            }
            match std::fs::read(&path) {
                Ok(contents) => {
                    self.files.insert(
                        stripped.to_path_buf(),
                        FileOwned {
                            contents: Contents::new(contents),
                            last_modified: meta.modified().unwrap_or(SystemTime::now()),
                        },
                    );
                }
                Err(_) => {
                    self.files.insert(
                        stripped.to_path_buf(),
                        FileOwned {
                            contents: Contents::Unreadable,
                            last_modified: meta.modified().unwrap_or(SystemTime::now()),
                        },
                    );
                }
            }
        }
    }

    pub fn file(&self, path: impl AsRef<Path>) -> Option<&Contents> {
        self.files
            .get(path.as_ref())
            .map(|v: &FileOwned| &v.contents)
    }

    pub fn file_mut(&mut self, path: impl AsRef<Path>) -> Option<&mut Contents> {
        self.dirty.insert(path.as_ref().to_path_buf());
        self.files.get_mut(path.as_ref()).map(|v| &mut v.contents)
    }
}
