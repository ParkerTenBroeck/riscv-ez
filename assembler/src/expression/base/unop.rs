use crate::{
    assembler::lang::AssemblyLanguage,
    context::{Node, NodeId},
    expression::{Constant, ExpressionEvaluator, NodeVal, Value, ValueType},
};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluator<'a, 'b, L> {
    pub fn unop_base(
        &mut self,
        node: NodeId<'a>,
        Node(op, _): Node<'a, UnOp>,
        mut expr: NodeVal<'a, L>,
        _: ValueType<'a, L>,
    ) -> Value<'a, L> {
        match op {
            UnOp::Neg => match &mut expr.0 {
                Value::Constant(c) => match c {
                    Constant::I8(i) => *i = i.wrapping_neg(),
                    Constant::I16(i) => *i = i.wrapping_neg(),
                    Constant::I32(i) => *i = i.wrapping_neg(),
                    Constant::I64(i) => *i = i.wrapping_neg(),
                    Constant::F32(i) => *i = -*i,
                    Constant::F64(i) => *i = -*i,
                    _ => self.invalid_unop(op, node, expr),
                },
                _ => self.invalid_unop(op, node, expr),
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
                    _ => self.invalid_unop(op, node, expr),
                },
                _ => self.invalid_unop(op, node, expr),
            },
        }
        expr.0
    }

    pub fn invalid_unop(&mut self, op: UnOp, node: NodeId<'a>, expr: NodeVal<'a, L>) {
        match op {
            UnOp::Neg => self.context.report_error(
                node,
                format!("Cannot negate expression of type {}", expr.0.get_type()),
            ),
            UnOp::Not => self.context.report_error(
                node,
                format!("Cannot not expression of type {}", expr.0.get_type()),
            ),
        }
    }
}
