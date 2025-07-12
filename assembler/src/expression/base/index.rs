use crate::expression::value::Indexed;
use crate::{
    assembler::AssemblyLanguage,
    context::{Node, NodeId},
    expression::{ExpressionEvaluator, ExpressionEvaluatorContext, NodeVal},
};

impl<'a, 'b, L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>
    ExpressionEvaluator<'a, 'b, L, T>
{
    pub fn index_base(
        &mut self,
        lhs: NodeVal<'a, L>,
        rhs: NodeVal<'a, L>,
        closing: NodeId<'a>,
    ) -> NodeVal<'a, L> {
        let node = self.context().merge_nodes(lhs.1, closing);

        Node(L::Indexed::from_indexed(self.0, node, lhs, rhs), node)
    }
}
