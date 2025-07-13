use std::fmt::Write;

use crate::{
    assembler::lang::AssemblyLanguage,
    context::{Node, NodeId},
    expression::{Constant, ExpressionEvaluator, ExpressionEvaluatorContext, NodeVal, Value},
};

impl<'a, 'b, L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>
    ExpressionEvaluator<'a, 'b, L, T>
{
    pub fn func_base(
        &mut self,
        func: &'a str,
        func_node: NodeId<'a>,
        args: Vec<NodeVal<'a, L>>,
        args_node: NodeId<'a>,
    ) -> NodeVal<'a, L> {
        let node = self.context().merge_nodes(func_node, args_node);
        let value = match func {
            "format" => 'result: {
                let mut result = String::new();
                let len = args.len();
                let mut iter = args.into_iter();
                match iter.next() {
                    Some(Node(Value::Constant(Constant::String(format)), _)) => {
                        let expected = format.matches('%').count();
                        if expected != len - 1 {
                            self.context().report_error(
                                args_node,
                                format!(
                                    "wrong number of arguments provided expected {} found {}",
                                    expected,
                                    len - 1
                                ),
                            )
                        }
                        for part in format.split('%') {
                            result
                                .write_fmt(format_args!(
                                    "{}{}",
                                    part,
                                    iter.next()
                                        .map(|e| e.0)
                                        .unwrap_or(Value::Constant(Constant::String("")))
                                ))
                                .unwrap();
                        }
                        break 'result Value::Constant(Constant::String(
                            self.context().alloc_str(result),
                        ));
                    }
                    Some(Node(v, node)) => self.context().report_error(
                        node,
                        format!("expected string literal found {}", v.get_type()),
                    ),
                    None => self
                        .context()
                        .report_error(func_node, "expected string literal found nothing"),
                }

                Value::Constant(Constant::String(""))
            }
            _ => {
                self.context()
                    .report_error(func_node, format!("Unknown function {func}"));
                Value::Constant(Constant::I32(0))
            }
        };
        Node(value, node)
    }
}
