use std::{ffi::OsStr, path::PathBuf};

pub mod code_editor;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Tab {
    CodeEditor(PathBuf),
    MemoryEditor,
    Log,
    Terminal,
    Display,
    CPU,
    Settings,
}

impl Tab {
    pub fn str(&self) -> &str {
        match self {
            Tab::CodeEditor(title) => title
                .file_name()
                .and_then(OsStr::to_str)
                .unwrap_or("INVALID"),
            Tab::MemoryEditor => "memory",
            Tab::Log => "log",
            Tab::Terminal => "terminal",
            Tab::Display => "display",
            Tab::CPU => "cpu",
            Tab::Settings => "settings",
        }
    }
}
