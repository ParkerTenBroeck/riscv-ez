use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
    time::{Duration, SystemTime},
};

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
    fn new(contents: Vec<u8>) -> Self {
        todo!()
    }
}

#[derive(Clone, Debug)]
pub struct FileOwned {
    pub contents: Contents,
    pub last_modified: SystemTime,
}

pub struct ProjectFiles {
    project: Option<PathBuf>,
    files: BTreeMap<PathBuf, FileOwned>,
    dirty: BTreeSet<PathBuf>,
}

impl ProjectFiles {
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

    pub fn sync(&mut self) {
        let Some(path) = self.project.clone() else {
            return;
        };
        self.explore(path);
    }

    fn explore(&mut self, path: PathBuf) {
        let Ok(meta) = path.metadata() else {
            return;
        };
        if meta.is_dir() {
            self.files.insert(
                path.to_path_buf(),
                FileOwned {
                    contents: Contents::Directory,
                    last_modified: meta.modified().unwrap_or(SystemTime::now()),
                },
            );
            for entry in path.read_dir().into_iter().flatten().flatten() {
                self.explore(entry.path());
            }
        } else if path.is_file() {
            if meta.modified().unwrap_or(SystemTime::UNIX_EPOCH)
                <= self
                    .files
                    .get(&path)
                    .map(|v| v.last_modified)
                    .unwrap_or(SystemTime::UNIX_EPOCH)
            {
                return;
            }
            match std::fs::read(&path) {
                Ok(contents) => {
                    self.files.insert(
                        path,
                        FileOwned {
                            contents: Contents::new(contents),
                            last_modified: meta.modified().unwrap_or(SystemTime::now()),
                        },
                    );
                }
                Err(_) => {
                    self.files.insert(
                        path,
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
        self.files.get_mut(path.as_ref()).map(|v| &mut v.contents)
    }
}
