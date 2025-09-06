use std::ffi::OsStr;
use std::path::{Path, PathBuf};

use egui::{Response, RichText};
use egui_ltreeview::{Action, NodeBuilder, TreeView, TreeViewBuilder, TreeViewState};

use crate::files;
use crate::files::*;

enum State {
    None,
    Creating {
        creation_path: PathBuf,
        name: String,
        dir: bool,
        first_frame: bool,
    },
    Renaming {
        og_path: PathBuf,
        name: String,
        first_frame: bool,
    },
}

pub struct FileTree {
    tree: TreeViewState<PathBuf>,
    state: State,
}

impl Default for FileTree {
    fn default() -> Self {
        Self::new()
    }
}

pub enum FileTreeAction {
    Delete(PathBuf),
    CreateFile(PathBuf),
    CreateDir(PathBuf),
    Tree(Action<PathBuf>),
    Rename(PathBuf, PathBuf),
}

fn tree_view_builder(
    builder: &mut TreeViewBuilder<'_, PathBuf>,
    files: &ProjectFiles,
    state: &mut State,
    ctx: &egui::Context,
) -> Vec<FileTreeAction> {
    let mut actions = Vec::new();
    let mut count = 0;
    for (path, is_dir) in files.paths() {
        let now = path.components().count() + 1 - !is_dir as usize;
        for _ in now..count {
            builder.close_dir();
        }
        count = now;

        let status = files.status(path);
        let color = match status {
            files::FileStatus::Error => ctx.style().visuals.error_fg_color,
            files::FileStatus::Dirty => ctx.style().visuals.warn_fg_color,
            files::FileStatus::Saved => ctx.style().visuals.text_color(),
        };

        let disp = if path.components().count() == 0 {
            RichText::new(
                files
                    .project_path()
                    .and_then(Path::file_name)
                    .and_then(OsStr::to_str)
                    .unwrap_or("INVALID NAME"),
            )
            .color(ctx.style().visuals.strong_text_color())
        } else {
            RichText::new(
                path.file_name()
                    .and_then(|v| v.to_str())
                    .unwrap_or("INVALID NAME"),
            )
            .color(color)
        };

        if let State::Renaming { og_path, name, first_frame } = state
            && og_path == path
        {
            let mut lost_focus = false;
            if is_dir {
                builder.node(NodeBuilder::dir(path.clone()).label_ui(|ui| {
                    let res = ui.text_edit_singleline(name);
                    if *first_frame{
                        res.request_focus();
                        *first_frame = false;
                    }
                    lost_focus = res.lost_focus();
                    if lost_focus {
                        let enter = ctx.input(|i| i.key_pressed(egui::Key::Enter));

                        if enter && og_path.file_name().and_then(OsStr::to_str) == Some(name.as_str()){
                            actions.push(FileTreeAction::Rename(
                                og_path.to_path_buf(),
                                og_path.parent().unwrap_or("".as_ref()).join(name.as_str()),
                            ));
                        }
                    }
                }));
            } else {
                builder.node(NodeBuilder::leaf(path.clone()).label_ui(|ui| {
                    let res = ui.text_edit_singleline(name);
                    if *first_frame{
                        res.request_focus();
                        *first_frame = false;
                    }
                    lost_focus = res.lost_focus();
                    if lost_focus {
                        let enter = ctx.input(|i| i.key_pressed(egui::Key::Enter));
                        if enter && og_path.file_name().and_then(OsStr::to_str) == Some(name.as_str()){
                            actions.push(FileTreeAction::Rename(
                                og_path.to_path_buf(),
                                og_path.parent().unwrap_or("".as_ref()).join(name.as_str()),
                            ));
                        }
                    }
                }));
            }
            if lost_focus {
                *state = State::None;
            }
            continue;
        }

        if is_dir {
            builder.node(
                NodeBuilder::dir(path.clone())
                    .label(disp)
                    .context_menu(|ui| {
                        if ui.button("create file").clicked() {
                            *state = State::Creating {
                                creation_path: path.to_path_buf(),
                                name: String::new(),
                                dir: false,
                                first_frame: true,
                            }
                        }
                        if ui.button("create dir").clicked() {
                            *state = State::Creating {
                                creation_path: path.to_path_buf(),
                                name: String::new(),
                                dir: true,
                                first_frame: true,
                            }
                        }
                        if ui.button("rename").clicked(){
                            *state = State::Renaming {
                                og_path: path.to_path_buf(),
                                name: path.file_name().unwrap_or_default().to_string_lossy().into_owned(),
                                first_frame: true,
                            }
                        }
                        if ui.button("delete").clicked() {
                            actions.push(FileTreeAction::Delete(path.to_path_buf()));
                        }
                    }),
            );
        } else {
            builder.node(
                NodeBuilder::leaf(path.clone())
                    .label(disp)
                    .context_menu(|ui| {
                        if ui.button("rename").clicked() {
                            *state = State::Renaming {
                                og_path: path.to_path_buf(),
                                name: path.file_name().unwrap_or_default().to_string_lossy().into_owned(),
                                first_frame: true,
                            }
                        }
                        if ui.button("delete").clicked() {
                            actions.push(FileTreeAction::Delete(path.to_path_buf()));
                        }
                    }),
            );
        }

        match state {
            State::Creating {
                creation_path,
                dir,
                name,
                first_frame,
            } if path == creation_path => {
                let enter = ctx.input(|i| i.key_pressed(egui::Key::Enter));
                let mut lost_focus = false;
                builder.node(NodeBuilder::leaf(path.join("#new#")).label_ui(|ui| {
                    let res = ui.text_edit_singleline(name);
                    if *first_frame {
                        res.request_focus();
                        *first_frame = false;
                    }
                    lost_focus = res.lost_focus();
                }));
                if lost_focus {
                    if enter {
                        if *dir {
                            actions.push(FileTreeAction::CreateDir(creation_path.join(name)));
                        } else {
                            actions.push(FileTreeAction::CreateFile(creation_path.join(name)));
                        }
                    }
                    *state = State::None;
                }
            }
            _ => {}
        }
    }
    for _ in 0..count {
        builder.close_dir();
    }
    actions
}

impl FileTree {
    pub fn new() -> Self {
        Self {
            tree: TreeViewState::default(),
            state: State::None,
        }
    }

    pub fn show(
        &mut self,
        ui: &mut egui::Ui,
        files: &ProjectFiles,
    ) -> (Response, Vec<FileTreeAction>) {
        let ctx = ui.ctx().clone();
        let mut actions = Vec::new();
        let (res, mut act) = TreeView::new(ui.make_persistent_id("file_tree")).show_state(
            ui,
            &mut self.tree,
            |builder| actions = tree_view_builder(builder, files, &mut self.state, &ctx),
        );
        act.drain(..)
            .map(FileTreeAction::Tree)
            .for_each(|a| actions.push(a));
        (res, actions)
    }
}
