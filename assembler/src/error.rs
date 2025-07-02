use crate::{
    context::{Source, SourceId},
    lex::Span,
};

use super::context::{Context, NodeId};

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    Error,
    Warning,
    Info,
    From,
}

pub struct ErrorPart<'a> {
    pub node: Option<NodeId<'a>>,
    pub source: Option<Source<'a>>,
    pub span: Option<Span>,
    pub kind: ErrorKind,
    pub msg: Option<String>,
}

#[derive(Default)]
pub struct FormattedError<'a> {
    pub parts: Vec<ErrorPart<'a>>,
}

impl<'a> FormattedError<'a> {
    pub fn new(
        context: &Context<'a>,
        node_id: NodeId<'a>,
        kind: ErrorKind,
        msg: impl Into<String>,
    ) -> Self {
        Self::default().add(context, node_id, kind, msg)
    }

    pub fn add(
        mut self,
        context: &Context<'a>,
        node_id: NodeId<'a>,
        kind: ErrorKind,
        msg: impl Into<String>,
    ) -> Self {
        let mut node_id = Some(node_id);
        let mut msg = Some(msg.into());
        let mut kind = kind;
        while let Some(node) = node_id {
            let node = *node;

            let src = *node.source;
            self.parts.push(ErrorPart {
                node: node_id,
                span: Some(node.span),
                source: Some(src),
                kind,
                msg: msg.take(),
            });
            kind = ErrorKind::From;

            node_id = node.invoked_by;
        }

        self
    }

    pub fn add_sourceless(&self, kind: ErrorKind, msg: String) -> FormattedError<'a> {
        let mut s = Self::default();
        s.parts.push(ErrorPart {
            node: None,
            source: None,
            span: None,
            kind,
            msg: Some(msg),
        });
        s
    }
}

const BOLD: &str = "\x1b[1m";
const RED: &str = "\x1b[31m";
const YELLOW: &str = "\x1b[33m";
const BLUE: &str = "\x1b[34m";
const GREEN: &str = "\x1b[32m";
const RESET: &str = "\x1b[0;22m";

impl<'a> std::fmt::Display for FormattedError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for part in self.parts.iter().rev() {
            match part.kind {
                ErrorKind::Error => write!(f, "{BOLD}{RED}error{RESET}{RESET}{BOLD}")?,
                ErrorKind::Warning => write!(f, "{BOLD}{YELLOW}warning{RESET}{RESET}{BOLD}")?,
                ErrorKind::Info => write!(f, "{BOLD}{BLUE}info{RESET}{RESET}{BOLD}")?,
                ErrorKind::From => write!(f, "{BOLD}{GREEN}from{RESET}{RESET}{GREEN}")?,
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
