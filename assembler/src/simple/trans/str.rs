use std::{collections::HashMap, num::NonZeroUsize, ops::Index};

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct StrIdx(NonZeroUsize);

#[derive(Clone, Default, Debug)]
pub struct StringTable {
    data: String,
    map: HashMap<String, StrIdx>,
}

impl StringTable {
    pub fn new() -> Self {
        Self {
            data: "\0".into(),
            map: HashMap::new(),
        }
    }

    pub fn resolve(&mut self, str: &str) -> StrIdx {
        if let Some(idx) = self.map.get(str) {
            return *idx;
        }
        let idx = StrIdx(
            NonZeroUsize::new(self.data.len())
                .expect("string table should always have a non zero length"),
        );
        self.data.push_str(str);
        self.data.push('\0');
        self.map.insert(str.into(), idx);
        idx
    }

    pub fn get(&self, idx: StrIdx) -> Option<&str> {
        let mut size = 0;
        while !matches!(self.data.as_bytes().get(idx.0.get() + size), Some(0) | None) {
            size += 1;
        }

        self.data.get(idx.0.get()..idx.0.get() + size)
    }
}

impl Index<StrIdx> for StringTable {
    type Output = str;

    fn index(&self, index: StrIdx) -> &Self::Output {
        self.get(index).unwrap()
    }
}
