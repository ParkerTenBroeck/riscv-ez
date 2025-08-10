use crate::tabs::{Tab, code_editor};
use egui_dock::TabViewer;
use poll_promise::Promise;
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};

#[derive(Debug, Hash, PartialEq, Eq, Clone, Ord, PartialOrd)]
pub struct ProjectFilePath {
    path: PathBuf,
    file: bool,
}

impl ProjectFilePath {
    pub fn path(&self) -> &Path {
        self.path.as_path()
    }

    pub fn is_file(&self) -> bool {
        self.file
    }
}

pub struct Context {
    pub project: PathBuf,
    files: BTreeMap<ProjectFilePath, Option<Vec<u8>>>,
}

impl Context {
    pub fn source_map(&self) -> HashMap<PathBuf, String> {
        self.files
            .iter()
            .filter_map(|(k, e)| {
                if k.file {
                    return None;
                }
                let e: String = String::from_utf8(e.as_ref()?.clone()).ok()?;
                Some((k.path.clone(), e))
            })
            .collect()
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

impl Context {
    pub fn new() -> Self {
        Self {
            project: "./test_files/".into(),
            files: BTreeMap::new(),
        }
    }

    pub fn spawn<T: Send + 'static>(
        &mut self,
        task: impl Send + 'static + FnOnce() -> T,
    ) -> Promise<T> {
        let (sender, receiver) = Promise::new();
        _ = std::thread::Builder::new()
            .name("asldkjas".into())
            .spawn(|| sender.send(task()));
        receiver
    }

    pub fn project_paths(&mut self) -> Vec<ProjectFilePath> {
        self.add_dir_rec(self.project.clone());
        self.files.iter().map(|i| i.0.clone()).collect()
    }

    fn add_dir_rec(&mut self, path: impl AsRef<Path>) {
        for entry in std::fs::read_dir(path).into_iter().flatten().flatten() {
            let path = entry.path();
            let path = path.strip_prefix(&self.project).unwrap().to_owned();
            match entry.file_type() {
                Ok(ft) if ft.is_dir() => {
                    self.files
                        .insert(ProjectFilePath { path, file: false }, None);
                    self.add_dir_rec(entry.path());
                }
                Ok(ft) if ft.is_file() => {
                    self.files
                        .insert(ProjectFilePath { path, file: true }, None);
                }
                _ => continue,
            }
        }
    }

    fn get_file(&mut self, path: &Path) -> Option<&mut Vec<u8>> {
        let nya = self
            .files
            .entry(ProjectFilePath {
                path: path.to_path_buf(),
                file: false,
            })
            .or_default();
        if nya.is_none() {
            let mut p = self.project.clone();
            p.push(path);
            *nya = std::fs::read(p).ok();
        }
        nya.as_mut()
    }
}

impl TabViewer for Context {
    type Tab = Tab;

    fn title(&mut self, title: &mut Tab) -> egui::WidgetText {
        egui::WidgetText::from(title.str())
    }

    fn ui(&mut self, ui: &mut egui::Ui, title: &mut Tab) {
        match title {
            Tab::CodeEditor(path) => {
                let text = self.get_file(path);
                if let Some(text) = text {
                    let mut str = Vec::new();
                    std::mem::swap(text, &mut str);
                    match String::from_utf8(str) {
                        Ok(mut str) => {
                            code_editor::show(ui, &mut str);
                            *text = str.into_bytes();
                        }
                        Err(error) => {
                            *text = error.into_bytes();
                        }
                    }
                }
            }
            Tab::MemoryEditor => {}
            Tab::Log => {}
            Tab::Terminal => {}
            Tab::Display => {}
            Tab::CPU => {}
            Tab::Settings => {}
        }
    }
}
