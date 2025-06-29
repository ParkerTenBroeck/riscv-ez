use std::{cell::RefCell, rc::Rc};

use crate::{
    assembler::{instructions::Instruction, translation::TranslationUnit},
    context::{Context, NodeId},
};
use crate::assembler::translation::{Label, Section};
use crate::error::{ErrorKind, FormattedError};

pub struct AssemblerContext<'a> {
    pub context: Rc<RefCell<Context<'a>>>,
    pub current_section: &'a str,
    pub current_label: &'a str,
    pub tu: TranslationUnit<'a, NodeId<'a>, SectionData<'a>>,
}

impl<'a> AssemblerContext<'a> {
    pub fn new(context: Rc<RefCell<Context<'a>>>) -> Self {
        AssemblerContext {
            context,
            current_label: "",
            current_section: "text",
            tu: Default::default(),
        }
    }
}

pub struct LabelD<'a>{
    pub name: &'a str,
    pub offset: i64,
}

pub enum SectionData<'a>{
    Instruction(Instruction<'a>),
    LabelAddress(LabelD<'a>),
    Space {
        size: u64,
        fill: u8,
    },
    Bytes(Vec<u8>)
}

impl<'a> SectionData<'a> {
    pub fn get_size(&self, ctx: &mut AssemblerContext) -> u64 {
        match self{
            SectionData::Instruction(_) => 4,
            SectionData::LabelAddress(_) => 4,
            SectionData::Space { size, .. } => *size,
            SectionData::Bytes(bytes) => bytes.len() as u64
        }
    }
}

impl<'a> AssemblerContext<'a> {
    pub fn add_instruction(&mut self, instruction: Instruction<'a>){
        self.add_data(SectionData::Instruction(instruction))
    }

    pub fn add_data(&mut self, data: SectionData<'a>){
        let size = data.get_size(self);
        let section = self.get_current_section();
        section.size += size;
        section.data.push(data);
        
        if let Some(label) = self.tu.labels.get_mut(self.current_label){
            label.size += size;
        }
    }

    pub fn get_current_section(&mut self) -> &mut Section<'a, SectionData<'a>>{
        self.tu.sections.entry(self.current_section).or_insert(Section{
            name: self.current_section,
            start: 0,
            size: 0,
            data: Vec::new(),
        })
    }

    pub fn add_label(&mut self, label: &'a str, source: NodeId<'a>){
        if let Some(previous) = self.tu.labels.get(label) {
            self.context.borrow_mut().report(|ctx| {
                FormattedError::default()
                    .add(ctx, source, ErrorKind::Error, "Label bound more than once")
                    .add(ctx, previous.source, ErrorKind::Info, "First bound here")
            });
            return;
        }
        self.current_label = label;
        self.tu.labels.insert(
            label,
            Label {
                source,
                section: self.current_section,
                offset: self
                    .tu
                    .sections
                    .get(self.current_section)
                    .map(|s| s.size)
                    .unwrap_or(0),
                size: 0,
            },
        );
    }
}
