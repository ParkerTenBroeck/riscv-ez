use crate::{
    assembler::{PreProcessorCtx, lang::AssemblyLanguage},
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
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        for token in self.lex.by_ref() {
            match token {
                Ok(ok) => {
                    return Some(Node(
                        ok.val,
                        ctx.context.node(NodeInfo {
                            span: ok.span,
                            source: self.source,
                            included_by: self.include_location,
                            invoked_by: None,
                        }),
                    ));
                }
                Err(err) => {
                    let n = ctx.context.node(NodeInfo {
                        span: err.span,
                        source: self.source,
                        included_by: self.include_location,
                        invoked_by: None,
                    });
                    ctx.context.report_error(n, err.val);
                }
            }
        }
        None
    }
}
