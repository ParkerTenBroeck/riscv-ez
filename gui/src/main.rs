#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

pub mod context;
pub mod files;
pub mod log;
pub mod tabs;

use crate::context::{Context, ProjectFilePath};
use crate::tabs::Tab;
use eframe::{NativeOptions, egui};
use egui::scroll_area::ScrollBarVisibility;
use egui::{CollapsingHeader, RichText, ScrollArea};
use egui_dock::{DockArea, DockState};
use egui_ltreeview::{Action, TreeView, TreeViewState};
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
    tree_view_state: TreeViewState<ProjectFilePath>,
    tree: DockState<Tab>,
}

impl Default for MyApp {
    fn default() -> Self {
        Self {
            context: Context::new("./test_files"),
            tree_view_state: Default::default(),
            tree: DockState::new(vec![]),
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
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
                        let tab_location = self.tree.find_tab(&tab);
                        let is_open = tab_location.is_some();
                        if ui
                            .selectable_label(is_open, RichText::new(tab.str()).strong())
                            .clicked()
                        {
                            if let Some(tab_location) = tab_location {
                                self.tree.set_active_tab(tab_location);
                            } else {
                                self.tree.push_to_focused_leaf(tab.clone());
                            }
                        }
                    }

                    if ui.button("assemble").clicked() {
                        let sources = self.context.source_map();
                        println!("{sources:#?}");
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
                                        context::FileContents::String(str) => Ok(
                                            riscv_asm::assembler::source::SourceContents::Text(str),
                                        ),
                                        context::FileContents::Bytes(items) => {
                                            Ok(riscv_asm::assembler::source::SourceContents::Bin(
                                                items,
                                            ))
                                        }

                                        context::FileContents::Unread => Err("hrrrmk".into()),
                                        context::FileContents::Unreadable => {
                                            Err("file does not exist or cannot be read".into())
                                        }
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

                    let state = TreeView::new(ui.make_persistent_id("nya")).show_state(
                        ui,
                        &mut self.tree_view_state,
                        |builder| {
                            let mut count = 0;
                            for path in self.context.project_paths() {
                                let now = path
                                    .path()
                                    .iter()
                                    .count()
                                    .saturating_sub(path.is_file() as usize);
                                for _ in now..count {
                                    builder.close_dir();
                                }
                                count = now;
                                let disp = path
                                    .path()
                                    .file_name()
                                    .and_then(|v| v.to_str())
                                    .unwrap_or("INVALID NAME")
                                    .to_owned();
                                if path.is_file() {
                                    builder.leaf(path, disp);
                                } else {
                                    builder.dir(path, disp);
                                }
                            }
                            builder.close_dir();
                        },
                    );

                    for action in state.1 {
                        match action {
                            Action::SetSelected(_) => {}
                            Action::Move(_) => {}
                            Action::Drag(_) => {}
                            Action::Activate(s) => {
                                for tab in s.selected {
                                    if !tab.is_file() {
                                        continue;
                                    }
                                    let tab = Tab::CodeEditor(tab.path().to_path_buf());
                                    let tab_location = self.tree.find_tab(&tab);
                                    if let Some(tab_location) = tab_location {
                                        self.tree.set_active_tab(tab_location);
                                    } else {
                                        self.tree.push_to_focused_leaf(tab.clone());
                                    }
                                }
                            }
                            Action::DragExternal(_) => {}
                            Action::MoveExternal(_) => {}
                        }
                    }
                });
        });

        egui::TopBottomPanel::bottom("logger").resizable(true).show(ctx, |ui|{
            CollapsingHeader::new("logs").default_open(true).show_unindented(ui, |ui|{
                self.context.terminal.show_framed(ui);
            })
        });

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(&ctx.style()).inner_margin(0.))
            .show(ctx, |ui| {
                DockArea::new(&mut self.tree).show_inside(ui, &mut self.context);
            });
    }
}
