#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

pub mod context;
pub mod files;
pub mod log;
pub mod tabs;
pub mod tree;

use crate::context::Context;
use crate::files::Contents;
use crate::tabs::Tab;
use crate::tree::FileTree;
use eframe::{NativeOptions, egui};
use egui::scroll_area::ScrollBarVisibility;
use egui::{CollapsingHeader, RichText, ScrollArea};
use egui_dock::{DockArea, DockState};
use egui_ltreeview::Action;
use riscv_asm::RiscvAssembler;
use riscv_asm::assembler::source::Sources;

fn main() -> eframe::Result<()> {
    let options = NativeOptions::default();
    eframe::run_native(
        "Text editor examples",
        options,
        Box::new(|_cc| Ok(Box::<MyApp>::default())),
    )
}

struct MyApp {
    context: Context,
    file_tree: FileTree,
    dock: DockState<Tab>,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            context: Context::new("./test_files"),
            file_tree: FileTree::new(),
            dock: DockState::new(vec![]),
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        if ctx.cumulative_frame_nr() % 128 == 0 {
            self.context.files.sync();
        }
        self.context.files.error_ui(ctx);

        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::MenuBar::new().ui(ui, |ui| {
                ui.menu_button("File", |ui| {
                    if ui.button("Quit").clicked() {
                        todo!()
                    }
                });
                ui.menu_button("View", |_ui| {});
                ui.menu_button("Help", |_ui| {});
                ui.menu_button("Settings", |_ui| {});
            })
        });
        egui::SidePanel::left("documents").show(ctx, |ui| {
            ScrollArea::both()
                .scroll_bar_visibility(ScrollBarVisibility::VisibleWhenNeeded)
                .show(ui, |ui| {
                    for tab in [Tab::Terminal, Tab::Display, Tab::CPU, Tab::Log] {
                        let tab_location = self.dock.find_tab(&tab);
                        let is_open = tab_location.is_some();
                        if ui
                            .selectable_label(is_open, RichText::new(tab.str()).strong())
                            .clicked()
                        {
                            if let Some(tab_location) = tab_location {
                                self.dock.set_active_tab(tab_location);
                            } else {
                                self.dock.push_to_focused_leaf(tab.clone());
                            }
                        }
                    }

                    if ui.button("assemble").clicked() {
                        let sources = self.context.source_map();
                        _ = Context::spawn(move || {});
                        let res = riscv_asm::assembler::assemble(
                            &Default::default(),
                            RiscvAssembler::default(),
                            Default::default(),
                            Default::default(),
                            "test.asm".as_ref(),
                            Sources::new(Box::new(|path, _| {
                                if let Some(src) = sources.get(path) {
                                    match src {
                                        Contents::String(str) => Ok(
                                            riscv_asm::assembler::source::SourceContents::Text(str),
                                        ),
                                        Contents::Bytes(items) => {
                                            Ok(riscv_asm::assembler::source::SourceContents::Bin(
                                                items,
                                            ))
                                        }

                                        Contents::Unread => Err("hrrrmk".into()),
                                        Contents::Unreadable => Err("file cannot be read".into()),
                                        Contents::Directory => Err("directory not a file".into()),
                                    }
                                } else {
                                    Err("file does not exist".into())
                                }
                            })),
                        );

                        use std::fmt::Write;
                        writeln!(&mut self.context.terminal, "{res}").unwrap();
                        writeln!(&mut self.context.terminal, "\n{}", res.output).unwrap();
                    }

                    ui.separator();
                    ui.label(RichText::new("Project Files").strong());

                    let (_, actions) = self.file_tree.show(ui, &self.context.files);
                    for action in actions {
                        match action {
                            tree::FileTreeAction::Delete(path) => self.context.files.delete(path),
                            tree::FileTreeAction::CreateFile(path) => {
                                self.context.files.create_file(path)
                            }
                            tree::FileTreeAction::CreateDir(path) => {
                                self.context.files.create_dir(path)
                            }

                            tree::FileTreeAction::Rename(_, _) => {}
                            tree::FileTreeAction::Tree(action) => match action {
                                Action::SetSelected(_) => {}
                                Action::Move(_) => {}
                                Action::Drag(_) => {}
                                Action::Activate(s) => {
                                    for tab in s.selected {
                                        if matches!(
                                            self.context.files.file(&tab),
                                            Some(Contents::Directory)
                                        ) {
                                            continue;
                                        }
                                        let tab = Tab::CodeEditor(tab.to_path_buf());
                                        let tab_location = self.dock.find_tab(&tab);
                                        if let Some(tab_location) = tab_location {
                                            self.dock.set_active_tab(tab_location);
                                        } else {
                                            self.dock.push_to_focused_leaf(tab.clone());
                                        }
                                    }
                                }
                                Action::DragExternal(_) => {}
                                Action::MoveExternal(_) => {}
                            },
                        }
                    }
                });
        });

        egui::TopBottomPanel::bottom("logger")
            .resizable(true)
            .default_height(ctx.available_rect().height() / 3.0)
            .show(ctx, |ui| {
                CollapsingHeader::new("logs")
                    .default_open(true)
                    .show_unindented(ui, |ui| {
                        self.context.show_terminal(ui);
                    })
            });

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(&ctx.style()).inner_margin(0.))
            .show(ctx, |ui| {
                DockArea::new(&mut self.dock).show_inside(ui, &mut self.context);
            });
    }
}
