use crate::{context::Source, lex::Span};

use super::context::NodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogKind {
    Error,
    Warning,
    Info,
    Hint,
    From,
}

pub struct LogPart<'a> {
    pub node: Option<NodeId<'a>>,
    pub source: Option<Source<'a>>,
    pub span: Option<Span>,
    pub kind: LogKind,
    pub msg: Option<String>,
}

#[derive(Default)]
pub struct LogEntry<'a> {
    pub parts: Vec<LogPart<'a>>,
}

impl<'a> LogEntry<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn error(self, node_id: NodeId<'a>, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Error, msg)
    }

    pub fn warning(self, node_id: NodeId<'a>, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Warning, msg)
    }

    pub fn info(self, node_id: NodeId<'a>, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Info, msg)
    }

    pub fn hint(self, node_id: NodeId<'a>, msg: impl ToString) -> Self {
        self.add(node_id, LogKind::Hint, msg)
    }

    pub fn add(mut self, node_id: NodeId<'a>, kind: LogKind, msg: impl ToString) -> Self {
        let mut node_id = Some(node_id);
        let mut msg = Some(msg.to_string());
        let mut kind = kind;

        let position = self.parts.len();
        while let Some(node) = node_id {
            let node = *node;

            let src = *node.source;
            self.parts.insert(
                position,
                LogPart {
                    node: node_id,
                    span: Some(node.span),
                    source: Some(src),
                    kind,
                    msg: msg.take(),
                },
            );
            if position != 0{
                break;
            }
            kind = LogKind::From;

            node_id = node.invoked_by;
        }

        self
    }

    pub fn error_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Error, msg)
    }

    pub fn warning_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Warning, msg)
    }

    pub fn info_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Info, msg)
    }

    pub fn hint_locless(self, msg: impl ToString) -> Self {
        self.add_locless(LogKind::Hint, msg)
    }

    pub fn add_locless(mut self, kind: LogKind, msg: impl ToString) -> LogEntry<'a> {
        self.parts.push(LogPart {
            node: None,
            source: None,
            span: None,
            kind,
            msg: Some(msg.to_string()),
        });
        self
    }
}

pub const BOLD: &str = "\x1b[1m";
pub const RED: &str = "\x1b[31m";
pub const YELLOW: &str = "\x1b[33m";
pub const BLUE: &str = "\x1b[34m";
pub const GREEN: &str = "\x1b[32m";
pub const RESET: &str = "\x1b[0;22m";

impl<'a> std::fmt::Display for LogEntry<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for part in self.parts.iter() {
            match part.kind {
                LogKind::Error => write!(f, "{BOLD}{RED}error{RESET}{RESET}{BOLD}")?,
                LogKind::Warning => write!(f, "{BOLD}{YELLOW}warning{RESET}{RESET}{BOLD}")?,
                LogKind::Info => write!(f, "{BOLD}{BLUE}info{RESET}{RESET}{BOLD}")?,
                LogKind::From => write!(f, "{BOLD}{GREEN}from{RESET}{RESET}{BOLD}")?,
                LogKind::Hint => write!(f, "{BOLD}{GREEN}hint{RESET}{RESET}{BOLD}")?,
            }

            if let Some(msg) = &part.msg {
                write!(f, ": {msg}")?;
            }

            let (Some(span), Some(source)) = (part.span, part.source) else {
                writeln!(f)?;
                continue;
            };

            let error_range = span.offset as usize..(span.offset as usize + span.len as usize);
            let start = source.contents[..error_range.start]
                .char_indices()
                .rev()
                .find_map(|c| (c.1 == '\n').then_some(c.0.saturating_add(1)))
                .unwrap_or(0);
            let to_end = if error_range.end < source.contents.len() {
                &source.contents[error_range.end..]
            } else {
                Default::default()
            };
            let end = error_range.end
                + to_end
                    .chars()
                    .position(|c| c == '\n')
                    .unwrap_or(to_end.len());
            let expanded_range = start..end;
            let expanded = &source.contents
                [expanded_range.start..expanded_range.end.min(source.contents.len())];

            let line = span.line + 1;
            let space = (((line as usize + expanded.lines().count()) as f32)
                .log10()
                .floor() as u8) as usize
                + 1;

            writeln!(
                f,
                "{BLUE}{BOLD}\n{: >space$}---> {RESET}{}:{}:{}",
                " ",
                source.path,
                line,
                span.col + 1
            )?;
            writeln!(f, "{BLUE}{BOLD}{: >space$} |", "")?;
            let mut index = expanded_range.start;
            for (i, line_contents) in expanded.split('\n').enumerate() {
                writeln!(
                    f,
                    "{: >space$} |{RESET} {}",
                    line as usize + i,
                    &line_contents
                )?;
                write!(f, "{BLUE}{BOLD}{: >space$} | ", "")?;
                for c in line_contents.chars() {
                    if error_range.contains(&index) {
                        write!(f, "~")?;
                    } else {
                        write!(f, " ")?;
                    }
                    index += c.len_utf8();
                }
                //nl
                if error_range.contains(&index) || error_range.is_empty() {
                    write!(f, "~")?;
                } else {
                    write!(f, " ")?;
                }
                index += '\n'.len_utf8();
                writeln!(f)?;
            }
            write!(f, "{RESET}")?
        }
        Ok(())
    }
}