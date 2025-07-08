use crate::{context::Source, lex::Span};

use super::context::NodeId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogKind {
    Error,
    Warning,
    Info,
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
    pub fn new(node_id: NodeId<'a>, kind: LogKind, msg: impl Into<String>) -> Self {
        Self::default().add(node_id, kind, msg)
    }

    pub fn add(mut self, node_id: NodeId<'a>, kind: LogKind, msg: impl Into<String>) -> Self {
        let mut node_id = Some(node_id);
        let mut msg = Some(msg.into());
        let mut kind = kind;
        while let Some(node) = node_id {
            let node = *node;

            let src = *node.source;
            self.parts.push(LogPart {
                node: node_id,
                span: Some(node.span),
                source: Some(src),
                kind,
                msg: msg.take(),
            });
            kind = LogKind::From;

            node_id = node.invoked_by;
        }

        self
    }

    pub fn add_sourceless(&self, kind: LogKind, msg: String) -> LogEntry<'a> {
        let mut s = Self::default();
        s.parts.push(LogPart {
            node: None,
            source: None,
            span: None,
            kind,
            msg: Some(msg),
        });
        s
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
        for part in self.parts.iter().rev() {
            match part.kind {
                LogKind::Error => write!(f, "{BOLD}{RED}error{RESET}{RESET}{BOLD}")?,
                LogKind::Warning => write!(f, "{BOLD}{YELLOW}warning{RESET}{RESET}{BOLD}")?,
                LogKind::Info => write!(f, "{BOLD}{BLUE}info{RESET}{RESET}{BOLD}")?,
                LogKind::From => write!(f, "{BOLD}{GREEN}from{RESET}{RESET}{GREEN}")?,
            }

            if let Some(msg) = &part.msg {
                write!(f, ": {msg}")?;
            }

            let (Some(span), Some(source)) = (part.span, part.source) else {
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
