use crate::expression::{AsmStr, ValueType};
use crate::{
    assembler::lang::AssemblyLanguage,
    context::{Node, NodeId},
    expression::{Constant, ExpressionEvaluator, NodeVal, Value},
};

impl<'a, 'b, L: AssemblyLanguage<'a>> ExpressionEvaluator<'a, 'b, L> {
    pub fn cast_base(
        &mut self,
        node: NodeId<'a>,
        expr: NodeVal<'a, L>,
        _: NodeId<'a>,
        ty: Node<'a, &'a str>,
        _: ValueType<'a, L>,
    ) -> Value<'a, L> {
        let ty = ty.0;
        use num_traits::AsPrimitive;
        macro_rules! integer {
            ($ident:ident, $ty:ty) => {{
                use Constant as C;
                use Value::Constant as V_C;
                match expr.0 {
                    V_C(C::I8(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I16(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I32(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I64(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I128(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::Isize(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::Iptr(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::U8(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U16(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U32(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U64(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U128(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::Usize(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::Uptr(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::F32(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::F64(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::Bool(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::Char(i)) => V_C(C::$ident(i as $ty)),
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            }};
        }

        macro_rules! float {
            ($ident:ident, $ty:ty) => {{
                use Constant as C;
                use Value::Constant as V_C;

                match expr.0 {
                    V_C(C::I8(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I16(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I32(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I64(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::I128(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::Isize(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::Iptr(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::U8(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U16(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U32(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U64(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::U128(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::Usize(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::Uptr(i)) => V_C(C::$ident(i.as_())),
                    V_C(C::F32(i)) => V_C(C::$ident(i as $ty)),
                    V_C(C::F64(i)) => V_C(C::$ident(i as $ty)),
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            }};
        }

        macro_rules! asm_num {
            ($ident:ident) => {{
                use crate::assembler::lang::FromAsPrimitive;
                use Constant as C;
                use Value::Constant as V_C;
                match expr.0 {
                    V_C(C::I8(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I16(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I32(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I64(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::I128(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    // V_C(C::Isize(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    // V_C(C::Iptr(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U8(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U16(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U32(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U64(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::U128(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    // V_C(C::Usize(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    // V_C(C::Uptr(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::F32(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::F64(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Bool(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    V_C(C::Char(i)) => V_C(C::$ident(FromAsPrimitive::from_as(i))),
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            }};
        }

        match ty {
            "str" => Value::Constant(Constant::Str(AsmStr::Str(match expr.0 {
                Value::Constant(Constant::Str(AsmStr::Str(str))) => str,
                Value::Constant(Constant::Str(bytes)) => match str::from_utf8(bytes.as_bytes()) {
                    Ok(str) => str,
                    Err(_) => {
                        self.context
                            .report_error(node, "contents of byte/c string contain bytes which cannot be present in ordinary strings");
                        ""
                    }
                },
                other => self.context.alloc_str(format!("{other}")),
            }))),
            "cstr" => Value::Constant(Constant::Str(AsmStr::CStr(match expr.0 {
                Value::Constant(Constant::Str(str)) => str.as_bytes(),
                other => self.context.alloc_str(format!("{other}")).as_bytes(),
            }))),
            "bstr" => Value::Constant(Constant::Str(AsmStr::ByteStr(match expr.0 {
                Value::Constant(Constant::Str(str)) => str.as_bytes(),
                other => self.context.alloc_str(format!("{other}")).as_bytes(),
            }))),
            "i8" => integer!(I8, i8),
            "i16" => integer!(I16, i16),
            "i32" => integer!(I32, i32),
            "i64" => integer!(I64, i64),
            "i128" => integer!(I128, i128),
            "isize" => asm_num!(Isize),
            "iptr" => asm_num!(Iptr),
            "u8" => integer!(U8, u8),
            "u16" => integer!(U16, u16),
            "u32" => integer!(U32, u32),
            "u64" => integer!(U64, u64),
            "u128" => integer!(U128, u128),
            "usize" => asm_num!(Usize),
            "uptr" => asm_num!(Uptr),
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
                self.context
                    .report_error(node, format!("unknown type '{ty}'"));
                expr.0
            }
        }
    }

    pub fn cast_error(&mut self, expr: NodeVal<'a, L>, expected: ValueType<'a, L>) -> Value<'a, L> {
        self.context.report_error(
            expr.1,
            format!("cannot cast '{}' to '{}'", expr.0.get_type(), expected),
        );
        expected.default_value()
    }
}
