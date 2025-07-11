use crate::assembler::AssemblyLanguage;
use crate::assembler::translation::{Label, Section};
use crate::logs::{LogEntry, LogKind};
use crate::{
    assembler::translation::TranslationUnit,
    context::{Context, NodeId},
};

pub struct AssemblerState<'a, T: AssemblyLanguage<'a>> {
    pub context: Context<'a>,
    pub current_section: &'a str,
    pub current_label: &'a str,
    pub tu: TranslationUnit<'a, NodeId<'a>>,
    pub lang: T,
}

impl<'a, T: AssemblyLanguage<'a>> AssemblerState<'a, T> {
    pub fn new(lang: T, context: Context<'a>) -> Self {
        AssemblerState {
            context,
            current_label: "",
            current_section: "text",
            tu: Default::default(),
            lang,
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
            start
                + ((align as usize - (current_section.data.len() & (align as usize - 1)))
                    & (align as usize - 1)),
            0,
        );

        let data_start = current_section.data.len();
        current_section.data.resize(data_start + size as usize, 0);
        let size = data_start + size as usize;
        &mut current_section.data[data_start..size]
    }

    pub fn add_data_const<const N: usize>(&mut self, align: u32) -> &mut [u8; N] {
        let data = self.add_data(N as u32, align);
        if data.len() != N {
            // self.context.report_error_nodeless(format!("When trying to create const slice from slice of length {} into arr of size {N}", data.len()));
            // self.context.print_errors();
            panic!(
                "When trying to create const slice from slice of length {} into arr of size {N}",
                data.len()
            )
        } else {
            match data.try_into() {
                Ok(value) => return value,
                Err(_) => unreachable!(),
            }
        }
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
            self.context.report(
                LogEntry::default()
                    .add(source, LogKind::Error, "Label bound more than once")
                    .add(previous.source, LogKind::Info, "First bound here"),
            );
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
