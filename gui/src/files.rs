use std::{
    collections::{BTreeMap, BTreeSet},
    io::Write,
    ops::{Deref, DerefMut},
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

    pub fn as_vec(self) -> Option<Vec<u8>> {
        match self {
            Contents::String(str) => Some(str.into()),
            Contents::Bytes(items) => Some(items),
            _ => None,
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
    pub modified: SystemTime,
}

#[derive(Debug, Default)]
pub struct ProjectFiles {
    project: Option<PathBuf>,
    files_working: BTreeMap<PathBuf, Contents>,
    files_cached: BTreeMap<PathBuf, FileOwned>,
    dirty: BTreeSet<PathBuf>,
    errors: BTreeMap<PathBuf, ErrorKind>,
}

#[derive(Debug)]
pub enum ErrorKind {
    FileContentsNewer,
    CannotOpenFile(std::io::Error),
    CannotSaveFile(std::io::Error),
    CannotCreateFile(std::io::Error),
    CannotCreateDirectory(std::io::Error),
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
        self.files_working.clear();
    }

    pub fn is_dirty(&self) -> bool {
        !self.dirty.is_empty()
    }

    pub fn is_path_dirty(&self, path: impl AsRef<Path>) -> bool {
        self.dirty.contains(path.as_ref())
    }

    fn create_file_(&mut self, path: impl AsRef<Path>, contents: &[u8]) {
        let Some(mut full_path) = self.project.clone() else {
            return;
        };
        if matches!(
            self.files_cached.get(path.as_ref()).map(|f| &f.contents),
            Some(Contents::Bytes(_) | Contents::String(_))
        ) {
            self.files_working.insert(
                path.as_ref().to_path_buf(),
                Contents::new(contents.to_owned()),
            );
            return;
        }
        full_path.extend(path.as_ref());

        if let Some(parent) = full_path.parent()
            && let Err(err) = std::fs::create_dir_all(parent)
        {
            self.errors.insert(
                path.as_ref().to_path_buf(),
                ErrorKind::CannotCreateDirectory(err),
            );
            return;
        }

        if let Err(err) = std::fs::write(&full_path, contents) {
            self.errors.insert(
                path.as_ref().to_path_buf(),
                ErrorKind::CannotCreateFile(err),
            );
        }

        self.explore(path);
    }

    fn create_dir_(&mut self, path: impl AsRef<Path>) {
        let Some(mut full_path) = self.project.clone() else {
            return;
        };
        if matches!(
            self.files_cached.get(path.as_ref()).map(|f| &f.contents),
            Some(Contents::Directory)
        ) {
            return;
        }
        full_path.extend(path.as_ref());

        if let Err(err) = std::fs::create_dir_all(&full_path) {
            self.errors.insert(
                path.as_ref().to_path_buf(),
                ErrorKind::CannotCreateDirectory(err),
            );
        }

        self.explore(path);
    }

    pub fn delete(&mut self, path: impl AsRef<Path>) {
        let path = path.as_ref();
        let Some(mut full_path) = self.project.clone() else {
            return;
        };
        full_path.extend(path);
        if full_path.is_file() {
            if let Err(err) = std::fs::remove_file(full_path) {
                todo!("{err}");
                // return;
            }
            self.dirty.remove(path);
            self.errors.remove(path);
            self.files_working.remove(path);
            self.files_cached.remove(path);
        } else if full_path.is_dir() {
            if let Err(err) = std::fs::remove_dir_all(full_path) {
                todo!("{err}");
                // return;
            }

            self.dirty.retain(|p| !p.starts_with(path));
            self.errors.retain(|p, _| !p.starts_with(path));
            self.files_working.retain(|p, _| !p.starts_with(path));
            self.files_cached.retain(|p, _| !p.starts_with(path));
        } else {
            todo!();
            // return;
        }
    }

    pub fn create_file(&mut self, path: impl AsRef<Path>) {
        self.create_file_(path, &[]);
    }

    pub fn create_dir(&mut self, path: impl AsRef<Path>) {
        self.create_dir_(path);
    }

    pub fn files(&self) -> impl Iterator<Item = (&PathBuf, &Contents)> {
        self.files_cached.iter().map(|(path, contents)| {
            if let Some(working) = self.files_working.get(path) {
                (path, working)
            } else {
                (path, &contents.contents)
            }
        })
    }

    pub fn paths(&self) -> impl Iterator<Item = (&PathBuf, bool)> {
        self.files_cached
            .iter()
            .map(|(path, contents)| {
                (
                    path,
                    matches!(contents.contents, Contents::Directory),
                )
            })
    }

    pub fn error_ui(&mut self, ctx: &egui::Context) {
        let mut remainder: BTreeMap<PathBuf, ErrorKind> = Default::default();
        std::mem::swap(&mut remainder, &mut self.errors);
        while let Some((path, error)) = remainder.pop_first() {
            Modal::new(format!("error {}", path.display()).into()).show(ctx, |ui| match &error {
                ErrorKind::FileContentsNewer => {
                    ui.label("File contents differ");
                    if ui.button("Overrite").clicked() {
                        let contents = self
                            .files_working
                            .get(&path)
                            .cloned()
                            .and_then(Contents::as_vec)
                            .unwrap_or_default();
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.create_file_(&path, &contents);
                        ui.close();
                        return;
                    }
                    if ui.button("Reload").clicked() {
                        self.files_working.remove(&path);
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.explore(path);
                        
                        ui.close();
                        return;
                    }
                    self.errors.insert(path, error);
                }
                ErrorKind::CannotOpenFile(err) => {
                    ui.label(format!("Cannot open file: {err}"));
                    if ui.button("Discard").clicked() {
                        self.files_working.remove(&path);
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.explore(&path);
                        ui.close();
                        return;
                    }
                    if ui.button("Create file").clicked() {
                        let contents = self
                            .files_working
                            .get(&path)
                            .cloned()
                            .and_then(Contents::as_vec)
                            .unwrap_or_default();
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.create_file_(&path, &contents);
                        ui.close();
                        return;
                    }
                    self.errors.insert(path, error);
                }
                ErrorKind::CannotSaveFile(err) => {
                    ui.label(format!("Cannot save file: {err}"));
                    if ui.button("Discard").clicked() {
                        self.files_working.remove(&path);
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        ui.close();
                        return;
                    }
                    if ui.button("Create file").clicked() {
                        let contents = self
                            .files_working
                            .get(&path)
                            .cloned()
                            .and_then(Contents::as_vec)
                            .unwrap_or_default();
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.create_file_(&path, &contents);
                        ui.close();
                        return;
                    }
                    self.errors.insert(path, error);
                }
                ErrorKind::CannotCreateFile(err) => {
                    ui.label(format!("Cannot create file: {err}"));
                    if ui.button("Discard").clicked() {
                        self.files_working.remove(&path);
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.explore(&path);
                        ui.close();
                        return;
                    }
                    if ui.button("Create file").clicked() {
                        let contents = self
                            .files_working
                            .get(&path)
                            .cloned()
                            .and_then(Contents::as_vec)
                            .unwrap_or_default();
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.create_file_(&path, &contents);
                        ui.close();
                        return;
                    }
                    self.errors.insert(path, error);
                }
                ErrorKind::CannotCreateDirectory(err) => {
                    ui.label(format!("Cannot create directory: {err}"));
                    if ui.button("Discard").clicked() {
                        self.files_working.remove(&path);
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.explore(&path);
                        ui.close();
                        return;
                    }
                    if ui.button("Create file").clicked() {
                        let contents = self
                            .files_working
                            .get(&path)
                            .cloned()
                            .and_then(Contents::as_vec)
                            .unwrap_or_default();
                        self.files_cached.remove(&path);
                        self.dirty.remove(&path);
                        self.create_file_(&path, &contents);
                        ui.close();
                        return;
                    }
                    self.errors.insert(path, error);
                }
            });
        }
    }

    pub fn sync(&mut self) {
        let Some(project) = self.project.clone() else {
            return;
        };
        for path in &self.dirty {
            if self.errors.contains_key(path) {
                continue;
            }
            let Some(working) = self.files_working.get(path) else {
                continue;
            };
            let Some(cached) = self.files_cached.get(path) else {
                continue;
            };
            let real_path = project.join(path);

            let contents = match &working {
                Contents::String(bytes) => bytes.as_bytes(),
                Contents::Bytes(items) => items,
                _ => continue,
            };
            let real_time = real_path
                .metadata()
                .ok()
                .and_then(|v| v.modified().ok())
                .unwrap_or(SystemTime::UNIX_EPOCH);
            let cached_time = cached.modified;
            if real_time > cached_time {
                self.errors
                    .insert(path.to_path_buf(), ErrorKind::FileContentsNewer);
                continue;
            }
            let mut file = match std::fs::OpenOptions::new()
                .write(true)
                .read(true)
                .open(&real_path)
            {
                Ok(file) => file,
                Err(err) => {
                    self.errors
                        .insert(path.to_path_buf(), ErrorKind::CannotOpenFile(err));
                    continue;
                }
            };
            if let Err(err) = file.write_all(contents) {
                self.errors
                    .insert(path.to_path_buf(), ErrorKind::CannotSaveFile(err));
                continue;
            }
            if let Err(err) = file.set_len(contents.len() as u64) {
                self.errors
                    .insert(path.to_path_buf(), ErrorKind::CannotSaveFile(err));
                continue;
            }
        }
        self.dirty.clear();
        self.explore("");
    }

    fn explore(&mut self, path: impl AsRef<Path>) {
        let path = path.as_ref();
        let Some(project) = &self.project else {
            return;
        };
        let full_path = project.join(path);
        let Ok(meta) = full_path.metadata() else {
            return;
        };
        
        if meta.is_dir() {
            self.files_cached.insert(
                path.to_path_buf(),
                FileOwned {
                    contents: Contents::Directory,
                    modified: meta.modified().unwrap_or(SystemTime::now()),
                },
            );
            for entry in full_path.read_dir().into_iter().flatten().flatten() {
                let Some(project) = &self.project else {
                    return;
                };
                if let Ok(path) = entry.path().strip_prefix(project){
                    self.explore(path);
                }
            }
        } else if full_path.is_file() {
            if self.errors.contains_key(path) {
                return;
            }
            if meta.modified().unwrap_or(SystemTime::UNIX_EPOCH)
                <= self
                    .files_cached
                    .get(path)
                    .map(|v| v.modified)
                    .unwrap_or(SystemTime::UNIX_EPOCH)
            {
                return;
            }
            if self.dirty.contains(path) {
                self.errors
                    .insert(path.to_path_buf(), ErrorKind::FileContentsNewer);
                return;
            }
            self.files_working.remove(path);
            match std::fs::read(&full_path) {
                Ok(contents) => {
                    self.files_cached.insert(
                        path.to_path_buf(),
                        FileOwned {
                            contents: Contents::new(contents),
                            modified: meta.modified().unwrap_or(SystemTime::now()),
                        },
                    );
                }
                Err(_) => {
                    self.files_cached.insert(
                        path.to_path_buf(),
                        FileOwned {
                            contents: Contents::Unreadable,
                            modified: meta.modified().unwrap_or(SystemTime::now()),
                        },
                    );
                }
            }
        }
        if let Some(parent) = path.parent() && !self.files_cached.contains_key(parent){
            self.explore(parent);
        }
    }

    pub fn file(&self, path: impl AsRef<Path>) -> Option<&Contents> {
        if let Some(contents) = self.files_working.get(path.as_ref()) {
            Some(contents)
        } else {
            self.files_cached.get(path.as_ref()).map(|f| &f.contents)
        }
    }

    pub fn file_mut(&mut self, path: impl AsRef<Path>) -> Option<ContentsMut<'_>> {
        use std::collections::btree_map::Entry;
        let cached = &self.files_cached.get(path.as_ref())?.contents;

        let working = match self.files_working.entry(path.as_ref().to_path_buf()) {
            Entry::Vacant(vacant_entry) => vacant_entry.insert(cached.clone()),
            Entry::Occupied(occupied_entry) => occupied_entry.into_mut(),
        };
        Some(ContentsMut {
            path: path.as_ref().to_path_buf(),
            working,
            cached,
            dirty: &mut self.dirty,
        })
    }

    pub(crate) fn status(&self, path: impl AsRef<Path>) -> FileStatus {
        if self.errors.contains_key(path.as_ref()) {
            FileStatus::Error
        } else if self.dirty.contains(path.as_ref()) {
            FileStatus::Dirty
        } else {
            FileStatus::Saved
        }
    }
}

pub struct ContentsMut<'a> {
    path: PathBuf,
    working: &'a mut Contents,
    cached: &'a Contents,
    dirty: &'a mut BTreeSet<PathBuf>,
}

impl<'a> Drop for ContentsMut<'a> {
    fn drop(&mut self) {
        if self.cached.as_bytes() != self.working.as_bytes() {
            self.dirty.insert(self.path.to_path_buf());
        }
    }
}

impl<'a> Deref for ContentsMut<'a> {
    type Target = Contents;

    fn deref(&self) -> &Self::Target {
        self.working
    }
}

impl<'a> DerefMut for ContentsMut<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.working
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FileStatus {
    Error,
    Dirty,
    Saved,
}
