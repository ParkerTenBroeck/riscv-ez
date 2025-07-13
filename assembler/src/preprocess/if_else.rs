use crate::{
    assembler::{context::AssemblerState, lang::AssemblyLanguage},
    context::{Node, NodeId},
    lex::Token,
    logs::LogEntry,
    preprocess::{FilterResult, PreProcessor, PreProcessorFilter},
};

pub struct IfDef<'a> {
    pub source: NodeId<'a>,
    pub condition: bool,
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
                        LogEntry::new()
                            .error(node, "Encountered #else multiple times")
                            .info(last, "first found here"),
                    );
                } else {
                    self.else_loc = Some(node);
                }
                FilterResult::Consume { remove: false }
            }
            None => {
                state.context.report(
                    LogEntry::new()
                        .error(state.context.top_src_eof(), "Expected #endif found eof")
                        .info(self.source, "from here"),
                );
                FilterResult::Consume { remove: true }
            }
            _ if !self.condition ^ self.else_loc.is_some() => {
                FilterResult::Consume { remove: false }
            }
            token => FilterResult::Pass {
                remove: false,
                token,
            },
        }
    }
}
