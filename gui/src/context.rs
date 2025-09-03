use crate::tabs::{Tab, code_editor};
use egui::{Color32, RichText, TextBuffer};
use egui_ansi::{Config, Terminal};
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

#[derive(Clone, Debug, Default)]
pub enum FileContents {
    String(String),
    Bytes(Vec<u8>),
    #[default]
    Unread,
    Unreadable,
}

impl FileContents {
    pub fn new(bytes: Vec<u8>) -> Self {
        match String::from_utf8(bytes) {
            Ok(str) => Self::String(str),
            Err(err) => Self::Bytes(err.into_bytes()),
        }
    }

    pub fn as_str(&mut self) -> Option<&str> {
        match self {
            FileContents::String(str) => Some(str),
            _ => None,
        }
    }

    pub fn as_string_mut(&mut self) -> Option<&mut String> {
        match self {
            FileContents::String(str) => Some(str),
            _ => None,
        }
    }

    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            FileContents::Bytes(items) => Some(items),
            FileContents::String(str) => Some(str.as_bytes()),
            FileContents::Unread => None,
            FileContents::Unreadable => None,
        }
    }
}

pub struct Context {
    pub project: PathBuf,
    files: BTreeMap<ProjectFilePath, FileContents>,
    pub terminal: Box<Terminal>,
}

impl Context {
    pub fn source_map(&self) -> HashMap<PathBuf, FileContents> {
        self.files
            .iter()
            .filter_map(|(k, e)| {
                if k.file {
                    return None;
                }
                Some((k.path.clone(), e.clone()))
            })
            .collect()
    }
}

impl Context {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        let path = path.into();
        Self {
            project: path,
            files: BTreeMap::new(),
            terminal: Terminal::new_box::<256>(Config::DARK),
        }
    }

    pub fn spawn<T: Send + 'static>(task: impl Send + 'static + FnOnce() -> T) -> Promise<T> {
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
                        .insert(ProjectFilePath { path, file: false }, FileContents::Unread);
                    self.add_dir_rec(entry.path());
                }
                Ok(ft) if ft.is_file() => {
                    self.files
                        .insert(ProjectFilePath { path, file: true }, FileContents::Unread);
                }
                _ => continue,
            }
        }
    }

    fn get_file(&mut self, path: &Path) -> &mut FileContents {
        let pfp = ProjectFilePath {
            path: path.to_path_buf(),
            file: true,
        };
        let entry = self.files.entry(pfp).or_default();
        if matches!(entry, FileContents::Unread) {
            let mut p = self.project.clone();
            p.push(path);
            *entry = match std::fs::read(p).map(FileContents::new) {
                Ok(contents) => contents,
                Err(_) => FileContents::Unreadable,
            }
        }
        entry
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
                if let Some(text) = text.as_string_mut() {
                    code_editor::show(ui, text);
                } else {
                    ui.label(RichText::new("file cannot be display as text").color(Color32::RED));
                }
            }
            Tab::MemoryEditor => {}
            Tab::Log => {
                self.terminal.show_framed(ui);
            }
            Tab::Terminal => {}
            Tab::Display => {}
            Tab::CPU => {}
            Tab::Settings => {}
        }
    }
}
