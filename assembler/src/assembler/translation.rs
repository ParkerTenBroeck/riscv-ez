use std::collections::HashMap;

pub struct TranslationUnit<'a, S, D: SectionData>{
    pub sections: HashMap<&'a str, Section<'a, D>>,
    pub labels: HashMap<&'a str, Label<'a, S>>,
}

impl<'a, S, D: SectionData> Default for TranslationUnit<'a, S, D>{
    fn default() -> Self {
        Self { sections: Default::default(), labels: Default::default() }
    }
}

pub struct Label<'a, S> {
    pub source: S,
    pub section: &'a str,
    pub offset: u64,
    pub size: u64,
}

pub struct Section<'a, D: SectionData> {
    pub name: &'a str,
    pub start: u64,
    pub size: u64,
    pub data: Vec<D>,
}

impl<'a, D: SectionData> Section<'a, D>{
    // pub fn add(&mut self, data: D){
    //     self.size += data.size(ctx);
    //     self.data.push(data);
    // }

    
}

pub trait SectionData{
    type Context;
    fn size(&self, ctx: &mut Self::Context) -> u64;
    fn to_bytes(&self, buffer: &mut [u8], ctx: &mut Self::Context); 
}