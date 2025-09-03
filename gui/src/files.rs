use std::{collections::{BTreeMap, BTreeSet}, path::{Path, PathBuf}, time::Duration};



#[derive(Clone, Debug, Default)]
pub enum Contents {
    #[default]
    Unread,
    Unreadable,
    Directory,
    String(String),
    Bytes(Vec<u8>),
}

#[derive(Clone, Debug, Default)]
pub struct FileOwned{
    pub contents: Contents,
    pub last_modified: Duration, 
}

pub struct ProjectFiles{
    project: Option<PathBuf>,
    files: BTreeMap<PathBuf, FileOwned>,
    dirty: BTreeSet<PathBuf>
}

impl ProjectFiles{
    pub fn project_path(&self) -> Option<&Path>{
        self.project.as_deref()
    }

    pub fn set_project_path(&mut self, path: impl Into<PathBuf>) {
        self.project = Some(path.into());
    }

    pub fn close_project(&mut self){
        self.project = None;
        self.files.clear();
    }

    pub fn is_dirty(&self) -> bool{
        !self.dirty.is_empty()
    }

    pub fn is_path_dirty(&self, path: impl AsRef<Path>) -> bool{
        self.dirty.contains(path.as_ref())
    }

    pub fn create_file(&mut self, path: impl Into<PathBuf>){
        
    }

    pub fn create_dir(&mut self, path: impl Into<PathBuf>){
        
    }

    pub fn sync(&mut self) {

    }

    pub fn file(&self, path: impl AsRef<Path>) -> FileC{
        match self.files.get(path.as_ref())
    }

    pub fn file_mut(&mut self, path: impl AsRef<Path>) -> FileMut<'_>{
        let path = path.as_ref();
        if !self.dirty.contains(path){
            let mut path = path.to_path_buf();
            while !self.dirty.insert(path.clone()){
                path.pop();
            }
            
            
        }
        match self.files.get_mut(path).map(|f|&mut f.contents){
            Some(Contents::Bytes(bytes)) => FileMut::Bytes(bytes),
            Some(Contents::String(bytes)) => FileMut::String(bytes),
            Some(Contents::Directory) => FileMut::Directory,
            Some(Contents::Unread) => FileMut::None,
            Some(Contents::Unreadable) => FileMut::Unreadable,
            None => FileMut::None
        }
    }
}