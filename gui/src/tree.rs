use std::ffi::OsStr;
use std::path::{Path, PathBuf};

use egui::{Response, RichText};
use egui_ltreeview::{Action, NodeBuilder, TreeView, TreeViewBuilder, TreeViewState};

use crate::files;
use crate::files::*;

enum NewKind{
    None,
    Editing{
        creation_path: PathBuf,
        name: String,
        dir: bool,
    }
}

pub struct FileTree {
    tree: TreeViewState<PathBuf>,
    new: NewKind,
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
}

fn tree_view_builder(
    builder: &mut TreeViewBuilder<'_, PathBuf>,
    files: &ProjectFiles,
    new: &mut NewKind,
    ctx: &egui::Context,
) -> Vec<FileTreeAction> {
    let mut first_frame_new = false;
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

        if is_dir {
            builder.node(
                NodeBuilder::dir(path.clone())
                    .label(disp)
                    .context_menu(|ui| {
                        if ui.button("create file").clicked() {
                            first_frame_new = true;
                            *new = NewKind::Editing{creation_path: path.to_path_buf(), name: String::new(), dir: false}
                        }
                        if ui.button("create dir").clicked() {
                            first_frame_new = true;
                            *new = NewKind::Editing{creation_path: path.to_path_buf(), name: String::new(), dir: true}
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
                        if ui.button("delete").clicked() {
                            actions.push(FileTreeAction::Delete(path.to_path_buf()));
                        }
                    }),
            );
        }

        if let NewKind::Editing{creation_path, dir, name} = new && path == creation_path{
            let enter = ctx.input(|i|i.key_pressed(egui::Key::Enter));
            let mut lost_focus = false;
            builder.node(NodeBuilder::leaf(path.join("#new#")).label_ui(|ui|{
                let res = ui.text_edit_singleline(name);
                if first_frame_new {
                    res.request_focus();
                }
                lost_focus = res.lost_focus();
            }));
            if lost_focus{
                if enter{
                    if *dir{
                        actions.push(FileTreeAction::CreateDir(creation_path.join(name)));
                    }else{
                        actions.push(FileTreeAction::CreateFile(creation_path.join(name)));
                    }
                }
                *new = NewKind::None;
            }
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
            new: NewKind::None,
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
            |builder| actions = tree_view_builder(builder, files, &mut self.new, &ctx),
        );
        act.drain(..)
            .map(FileTreeAction::Tree)
            .for_each(|a| actions.push(a));
        (res, actions)
    }
}
