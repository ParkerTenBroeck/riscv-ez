use crate::{
    assembler::{AssemblyLanguage, context::AssemblerState},
    context::{Node, NodeId},
    logs::LogEntry,
    lex::Token,
    preprocess::{FilterResult, PreProcessor, PreProcessorFilter},
};

pub struct IfDef<'a> {
    pub source: NodeId<'a>,
    pub defined: bool,
    pub else_loc: Option<NodeId<'a>>,
}

impl<'a, T: AssemblyLanguage<'a>> PreProcessorFilter<'a, T> for IfDef<'a> {
    fn filter(
        &mut self,
        _: &mut PreProcessor<'a, T>,
        state: &mut AssemblerState<'a, T>,
        token: Option<Node<'a, Token<'a>>>,
    ) -> FilterResult<'a> {
        match token {
            Some(Node(Token::PreProcessorTag("endif"), _)) => {
                FilterResult::Consume { remove: true }
            }
            Some(Node(Token::PreProcessorTag("else"), node)) => {
                if let Some(last) = self.else_loc {
                    state.context.report(
                        LogEntry::new(
                            node,
                            crate::logs::LogKind::Error,
                            "Encountered #else multiple times",
                        )
                        .add(
                            last,
                            crate::logs::LogKind::Info,
                            "First found here",
                        ),
                    );
                } else {
                    self.else_loc = Some(node);
                }
                FilterResult::Consume { remove: false }
            }
            None => {
                state.context.report(
                    LogEntry::new(
                        state.context.top_src_eof(),
                        crate::logs::LogKind::Error,
                        "Expected #endif found eof",
                    )
                    .add(
                        self.source,
                        crate::logs::LogKind::Info,
                        "From here",
                    ),
                );
                FilterResult::Consume { remove: true }
            }
            _ if !self.defined ^ self.else_loc.is_some() => FilterResult::Consume { remove: false },
            token => FilterResult::Pass {
                remove: false,
                token,
            },
        }
    }
}
