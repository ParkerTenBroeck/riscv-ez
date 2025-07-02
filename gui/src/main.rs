#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")] // hide console window on Windows in release

mod context;
mod log;
mod tabs;

use crate::context::{Context, ProjectFilePath};
use crate::tabs::Tab;
use eframe::{NativeOptions, egui};
use egui::scroll_area::ScrollBarVisibility;
use egui::{RichText, ScrollArea};
use egui_dock::{DockArea, DockState, TabViewer};
use egui_ltreeview::{Action, TreeView, TreeViewState};

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
            context: Context::new(),
            tree_view_state: Default::default(),
            tree: DockState::new(vec![]),
        }
    }
}

impl eframe::App for MyApp {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            egui::menu::bar(ui, |ui| {
                ui.menu_button("File", |ui| {
                    ui.button("Quit");
                });
                ui.menu_button("View", |ui| {});
                ui.menu_button("Help", |ui| {});
                ui.menu_button("Settings", |ui| {});
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
                        _ = self.context.spawn(|| {
                            assembler::assemble_and_link(sources, vec!["test.asm"]);
                        });
                    }

                    ui.separator();
                    ui.label(RichText::new("Project Files").strong());

                    let mut context = Context::new();
                    let context = &mut context;
                    let state = TreeView::new(ui.make_persistent_id("nya")).show_state(
                        ui,
                        &mut self.tree_view_state,
                        |builder| {
                            let mut count = 0;
                            for path in context.project_paths() {
                                let now = path
                                    .str()
                                    .matches("/")
                                    .count()
                                    .saturating_sub(path.is_file() as usize);
                                for _ in now..count {
                                    builder.close_dir();
                                }
                                count = now;
                                let disp =
                                    path.str().split("/").last().unwrap_or("ERROR").to_owned();
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
                                    let tab = Tab::CodeEditor(tab.into_string());
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

        egui::CentralPanel::default()
            .frame(egui::Frame::central_panel(&ctx.style()).inner_margin(0.))
            .show(ctx, |ui| {
                DockArea::new(&mut self.tree).show_inside(ui, &mut self.context);
            });
    }
}
