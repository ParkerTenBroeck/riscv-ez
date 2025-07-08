use crate::{
    assembler::{AssemblyLanguage, context::AssemblerState},
    context::{Node, NodeId, NodeInfo, SourceId},
    lex::{Lexer, Token},
    preprocess::{PreProcessor, PreProcessorIter},
};

pub struct FileIter<'a> {
    pub lex: Lexer<'a>,
    pub source: SourceId<'a>,
    pub include_location: Option<NodeId<'a>>,
}

impl<'a, T: AssemblyLanguage<'a>> PreProcessorIter<'a, T> for FileIter<'a> {
    fn next(
        &mut self,
        _: &mut PreProcessor<'a, T>,
        state: &mut AssemblerState<'a, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        for token in self.lex.by_ref() {
            match token {
                Ok(ok) => {
                    return Some(Node(
                        ok.val,
                        state.context.node(NodeInfo {
                            span: ok.span,
                            source: self.source,
                            included_by: self.include_location,
                            invoked_by: None,
                        }),
                    ));
                }
                Err(err) => {
                    let n = state.context.node(NodeInfo {
                        span: err.span,
                        source: self.source,
                        included_by: self.include_location,
                        invoked_by: None,
                    });
                    state.context.report_error(n, err.val);
                }
            }
        }
        None
    }
}
