use crate::assembler::translation::{CalculationKind, FormKind, Label, Relocation, Section};
use crate::error::{ErrorKind, FormattedError};
use crate::{
    assembler::translation::TranslationUnit,
    context::{Context, NodeId},
};
use std::rc::Rc;

pub struct AssemblerContext<'a> {
    pub context: Rc<Context<'a>>,
    pub current_section: &'a str,
    pub current_label: &'a str,
    pub tu: TranslationUnit<'a, NodeId<'a>>,
}

impl<'a> AssemblerContext<'a> {
    pub fn new(context: Rc<Context<'a>>) -> Self {
        AssemblerContext {
            context,
            current_label: "",
            current_section: "text",
            tu: Default::default(),
        }
    }

    pub fn set_current_section(&mut self, section: &'a str) {
        self.current_section = section;
        self.tu.sections.entry(section).or_insert(Section {
            name: section,
            start: None,
            align: 0,
            data: vec![],
            relocs: vec![],
        });
    }

    pub fn add_data(&mut self, size: u32, align: u32) -> &mut [u8] {
        if let Some(label) = self.tu.labels.get_mut(self.current_label) {
            label.size += size;
        }
        let current_section = self.get_current_section();

        let start = current_section.data.len();
        current_section.align = current_section.align.max(align);
        current_section.data.resize(
            (align as usize - (current_section.data.len() & (align as usize - 1)))
                & (align as usize - 1),
            0,
        );

        let data_start = current_section.data.len();
        let size = start + size as usize;
        current_section.data.resize(size, 0);
        &mut current_section.data[data_start..size]
    }

    pub fn add_data_const<const N: usize>(&mut self, align: u32) -> &mut [u8; N] {
        self.add_data(N as u32, align).try_into().unwrap()
    }

    pub fn ins_or_address_reloc(
        &mut self,
        data: u32,
        label: &'a str,
        offset: i32,
        calc: CalculationKind,
        form: FormKind,
    ) {
        let sec = self.get_current_section();
        sec.relocs.push(Relocation {
            label,
            value_offset: offset,
            section_offset: sec.data.len() as u32,
            form,
            calc,
        });
        *self.add_data_const::<4>(4) = data.to_le_bytes();
    }

    pub fn get_current_section(&mut self) -> &mut Section<'a> {
        self.tu
            .sections
            .entry(self.current_section)
            .or_insert(Section {
                name: self.current_section,
                start: None,
                align: 0,
                data: Vec::new(),
                relocs: vec![],
            })
    }

    pub fn add_label(&mut self, label: &'a str, source: NodeId<'a>) {
        if let Some(previous) = self.tu.labels.get(label) {
            self.context.report(|ctx| {
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
                    .map(|s| s.data.len() as u32)
                    .unwrap_or(0),
                size: 0,
                align: 0,
            },
        );
    }
}
