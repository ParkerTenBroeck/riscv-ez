use crate::{
    assembler::AssemblyLanguage,
    context::{Node, NodeId},
    expression::{Constant, ExpressionEvaluator, ExpressionEvaluatorContext, NodeVal, Value},
};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

impl<'a, 'b, L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>
    ExpressionEvaluator<'a, 'b, L, T>
{
    pub fn unop_base(
        &mut self,
        op: UnOp,
        node: NodeId<'a>,
        mut expr: NodeVal<'a, L>,
    ) -> NodeVal<'a, L> {
        let node = self.context().merge_nodes(node, expr.1);
        match op {
            UnOp::Neg => match &mut expr.0 {
                Value::Constant(c) => match c {
                    Constant::I8(i) => *i = i.wrapping_neg(),
                    Constant::I16(i) => *i = i.wrapping_neg(),
                    Constant::I32(i) => *i = i.wrapping_neg(),
                    Constant::I64(i) => *i = i.wrapping_neg(),
                    Constant::F32(i) => *i = -*i,
                    Constant::F64(i) => *i = -*i,
                    _ => self.context().report_error(
                        node,
                        format!("Cannot negate expression of type {}", expr.0.get_type()),
                    ),
                },
                _ => self.context().report_error(
                    node,
                    format!("Cannot negate expression of type {}", expr.0.get_type()),
                ),
            },
            UnOp::Not => match &mut expr.0 {
                Value::Constant(c) => match c {
                    Constant::I8(i) => *i = !*i,
                    Constant::I16(i) => *i = !*i,
                    Constant::I32(i) => *i = !*i,
                    Constant::I64(i) => *i = !*i,
                    Constant::U8(i) => *i = !*i,
                    Constant::U16(i) => *i = !*i,
                    Constant::U32(i) => *i = !*i,
                    Constant::U64(i) => *i = !*i,
                    Constant::Bool(i) => *i = !*i,
                    _ => self.context().report_error(
                        node,
                        format!("Cannot not expression of type {}", expr.0.get_type()),
                    ),
                },
                _ => self.context().report_error(
                    node,
                    format!("Cannot not expression of type {}", expr.0.get_type()),
                ),
            },
        }
        Node(expr.0, node)
    }

    pub fn invalid_unop(&mut self,
        op: UnOp,
        node: NodeId<'a>,
        expr: NodeVal<'a, L>){

        }
}
