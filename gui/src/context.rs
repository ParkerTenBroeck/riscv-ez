use crate::files::{Contents, ProjectFiles};
use crate::tabs::{Tab, code_editor};
use egui::{Color32, RichText, Sense};
use egui_ansi::{Config, Terminal};
use egui_dock::TabViewer;
use poll_promise::Promise;
use std::collections::HashMap;
use std::path::PathBuf;

pub struct Context {
    pub files: ProjectFiles,
    pub terminal: Box<Terminal>,
}

impl Context {
    pub fn source_map(&self) -> HashMap<PathBuf, Contents> {
        self.files
            .files()
            .map(|(k, e)| (k.clone(), e.clone()))
            .collect()
    }
}

impl Context {
    pub fn new(path: impl Into<PathBuf>) -> Self {
        Self {
            files: ProjectFiles::new(Some(path.into())),
            terminal: Terminal::new_box::<256>(Config::DARK),
        }
    }

    pub fn show_terminal(&mut self, ui: &mut egui::Ui) {
        egui::Frame::new()
            .inner_margin(2)
            .fill(self.terminal.cfg.bg_default)
            .show(ui, |ui| {
                egui::ScrollArea::both()
                    .stick_to_bottom(true)
                    .stick_to_right(true)
                    .show(ui, |ui| {
                        ui.label(self.terminal.layout()).context_menu(|ui| {
                            if ui.button("clear").clicked() {
                                self.terminal.clear();
                            }
                        });
                        ui.allocate_response(ui.available_size(), Sense::click())
                            .context_menu(|ui| {
                                if ui.button("clear").clicked() {
                                    self.terminal.clear();
                                }
                            });
                    });
            });
    }

    pub fn spawn<T: Send + 'static>(task: impl Send + 'static + FnOnce() -> T) -> Promise<T> {
        let (sender, receiver) = Promise::new();
        _ = std::thread::Builder::new()
            .name("asldkjas".into())
            .spawn(|| sender.send(task()));
        receiver
    }
}

impl TabViewer for Context {
    type Tab = Tab;

    fn title(&mut self, title: &mut Tab) -> egui::WidgetText {
        match title {
            Tab::CodeEditor(path) => {
                let status = self.files.status(path);
                let color = match status {
                    crate::files::FileStatus::Error => Color32::RED,
                    crate::files::FileStatus::Dirty => Color32::YELLOW,
                    crate::files::FileStatus::Saved => Color32::GRAY,
                };
                RichText::new(title.str()).color(color).into()
            }
            _ => egui::WidgetText::from(title.str()),
        }
    }

    fn ui(&mut self, ui: &mut egui::Ui, title: &mut Tab) {
        match title {
            Tab::CodeEditor(path) => {
                match self.files.file_mut(path).as_deref_mut() {
                    Some(Contents::Directory) => {
                        ui.label(RichText::new("directory not a file").color(Color32::RED));
                    }
                    Some(Contents::Unreadable) => {
                        ui.label(RichText::new("file cannot be read").color(Color32::RED));
                    }
                    Some(file) => {
                        if let Some(contents) = file.as_string_mut() {
                            code_editor::show(ui, contents);
                        } else {
                            ui.label(RichText::new("file is not utf-8").color(Color32::RED));
                        }
                    }
                    None => {
                        ui.label(RichText::new("File does not exist").color(Color32::RED));
                    }
                };
            }
            Tab::MemoryEditor => {}
            Tab::Log => {
                self.show_terminal(ui);
            }
            Tab::Terminal => {}
            Tab::Display => {}
            Tab::CPU => {}
            Tab::Settings => {}
        }
    }
}
