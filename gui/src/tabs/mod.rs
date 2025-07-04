
pub mod code_editor;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Tab {
    CodeEditor(String),
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
            Tab::CodeEditor(title) => title.as_str(),
            Tab::MemoryEditor => "memory",
            Tab::Log => "log",
            Tab::Terminal => "terminal",
            Tab::Display => "display",
            Tab::CPU => "cpu",
            Tab::Settings => "settings",
        }
    }
}
