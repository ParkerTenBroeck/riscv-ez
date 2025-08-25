#[derive(Clone)]
pub struct Data {
    data: Vec<u8>,
    align: usize,
}

impl Data {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            align: 1,
        }
    }

    pub fn current_offset(&self) -> u32 {
        self.data.len() as u32
    }

    pub fn push_data(&mut self, data: &[u8], align: usize) -> std::ops::Range<usize> {
        let start = self.data.len();

        self.align = self.align.max(align);
        self.data.extend_from_slice(data);

        let end = self.data.len();

        start..end
    }

    pub fn push_space(&mut self, space: usize, align: usize) -> std::ops::Range<usize> {
        let start = self.data.len();

        self.align = self.align.max(align);
        self.data.resize(start, 0);

        let end = self.data.len();

        start..end
    }
}
