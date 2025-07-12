use crate::expression::ValueType;
use crate::{
    assembler::AssemblyLanguage,
    context::{Node, NodeId},
    expression::{Constant, ExpressionEvaluator, ExpressionEvaluatorContext, NodeVal, Value},
};

impl<'a, 'b, L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>
    ExpressionEvaluator<'a, 'b, L, T>
{
    pub fn cast_base(
        &mut self,
        expr: NodeVal<'a, L>,
        ty: &'a str,
        node_id: NodeId<'a>,
    ) -> NodeVal<'a, L> {
        macro_rules! integer {
            ($ident:ident, $ty:ty) => {
                match expr.0 {
                    Value::Constant(Constant::I8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::Bool(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::Char(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            };
        }

        macro_rules! float {
            ($ident:ident, $ty:ty) => {
                match expr.0 {
                    Value::Constant(Constant::I8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            };
        }

        let node = self.context().merge_nodes(expr.1, node_id);
        let value = match ty {
            "str" => Value::Constant(Constant::String(
                self.context().alloc_str(format!("{}", expr.0).as_str()),
            )),
            "i8" => integer!(I8, i8),
            "i16" => integer!(I16, i16),
            "i32" => integer!(I32, i32),
            "i64" => integer!(I64, i64),
            "u8" => integer!(U8, u8),
            "u16" => integer!(U16, u16),
            "u32" => integer!(U32, u32),
            "u64" => integer!(U64, u64),
            "f32" => float!(F32, f32),
            "f64" => float!(F64, f64),
            "char" => match expr.0 {
                Value::Constant(Constant::U8(i)) => Value::Constant(Constant::Char(i as char)),
                Value::Constant(Constant::U16(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::U32(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::U64(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::Char(i)) => Value::Constant(Constant::Char(i)),
                _ => self.cast_error(expr, ValueType::Char),
            },
            _ => {
                self.context()
                    .report_error(node, format!("Unknown type {ty}"));
                expr.0
            }
        };
        Node(value, node)
    }

    pub fn cast_error(&mut self, expr: NodeVal<'a, L>, expected: ValueType<'a, L>) -> Value<'a, L> {
        self.context().report_error(
            expr.1,
            format!("Cannot cast {} to {}", expr.0.get_type(), expected),
        );
        expected.default_value()
    }
}
