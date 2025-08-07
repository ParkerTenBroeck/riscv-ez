use crate::{
    assembler::{PreProcessorCtx, lang::AssemblyLanguage},
    context::{Node, NodeId, NodeInfo},
    lex::Token,
    preprocess::{PreProcessor, PreProcessorIter},
};

pub struct TokenIter<'a> {
    pub toks: std::vec::IntoIter<Node<'a, Token<'a>>>,
    pub source: NodeId<'a>,
}

impl<'a, T: AssemblyLanguage<'a>> PreProcessorIter<'a, T> for TokenIter<'a> {
    fn next(
        &mut self,
        _: &mut PreProcessor<'a, T>,
        ctx: &mut PreProcessorCtx<'a, '_, T>,
    ) -> Option<Node<'a, Token<'a>>> {
        let tok = self.toks.next()?;

        Some(Node(
            tok.0,
            ctx.context.node(NodeInfo {
                span: tok.1.span,
                source: tok.1.source,
                parent: crate::context::Parent::Pasted {
                    parent: self.source,
                    definition: None,
                },
            }),
        ))
    }
}
