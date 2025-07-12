use crate::{
    assembler::AssemblyLanguage,
    context::NodeId,
    expression::{Constant, ExpressionEvaluator, ExpressionEvaluatorContext, Value, ValueType},
    lex::{Number, TypeHint},
};

use std::num::IntErrorKind;

impl<'a, 'b, L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>
    ExpressionEvaluator<'a, 'b, L, T>
{
    pub fn parse_numeric_literal_base(
        &mut self,
        num: Number<'a>,
        n: NodeId<'a>,
        hint: ValueType<'a, L>,
        negated: bool,
    ) -> Constant<'a> {
        let mut buf = [0u8; 32];
        let suffix = if let Some(suffix) = num.get_suffix() {
            let mut index = 0;
            for c in suffix.chars() {
                if c != '_' {
                    if index + c.len_utf8() > buf.len() {
                        self.context()
                            .report_error(n, format!("Unknown numeric suffix '{suffix}'"));
                        return if let Value::Constant(c) = hint.default_value() {
                            c
                        } else {
                            Constant::I32(0)
                        };
                    }
                    c.encode_utf8(&mut buf[index..]);
                    index += c.len_utf8();
                }
            }
            Some(str::from_utf8(&buf[..index]).unwrap())
        } else {
            None
        };

        let (suffix, radix) = match num.get_hint() {
            TypeHint::Float if hint.is_integer() => {
                (suffix.unwrap_or(L::DEFAULT_FLOAT_POSTFIX), 10)
            }
            TypeHint::Float => (
                suffix.unwrap_or(hint.numeric_suffix().unwrap_or(L::DEFAULT_FLOAT_POSTFIX)),
                10,
            ),
            TypeHint::Hex => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                16,
            ),
            TypeHint::Bin => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                2,
            ),
            TypeHint::Int => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                10,
            ),
            TypeHint::Oct => (
                suffix.unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or(L::DEFAULT_INTEGER_POSTFIX),
                ),
                8,
            ),
        };

        let negated = suffix.starts_with("i") && negated;

        let mut buf = [0u8; 256];
        let number = {
            let mut index = 0;
            if negated {
                buf[index] = b'-';
                index += 1;
            }

            for c in num.get_num().chars() {
                if c != '_' {
                    if index + c.len_utf8() > buf.len() {
                        self.context()
                            .report_error(n, format!("numeric literal too long"));
                        return if let Value::Constant(c) = hint.default_value() {
                            c
                        } else {
                            Constant::I32(0)
                        };
                    }
                    c.encode_utf8(&mut buf[index..]);
                    index += c.len_utf8();
                }
            }
            str::from_utf8(&buf[..index]).unwrap()
        };

        macro_rules! integer {
            ($num:ty) => {{
                let v = <$num>::from_str_radix(number, radix)
                    .inspect_err(|e| {
                        use crate::LogEntry;
                        match e.kind(){
                            IntErrorKind::NegOverflow => self.context().report(LogEntry::new().error(n, format!("numeric literal too small to fit in type {suffix} valid range is {}..={}", <$num>::MIN, <$num>::MAX)).hint_locless("consider adding an explicit type suffix to the literal like '<integer|float>i32' or '<integer|float>u16'")),
                            IntErrorKind::PosOverflow => self.context().report(LogEntry::new().error(n, format!("numeric literal too large to fit in type {suffix} valid range is {}..={}", <$num>::MIN, <$num>::MAX)).hint_locless("consider adding an explicit type suffix to the literal like '<integer|float>i32' or '<integer|float>u16'")),
                            _ => self.context().report_error(n, format!("Invalid numeric literal"))
                        }
                    })
                    .unwrap_or(0);
                if negated{
                    v.wrapping_neg()
                }else{
                    v
                }
            }};
        }

        macro_rules! float {
            ($num:ty) => {
                number
                    .parse()
                    .inspect_err(|e| {
                        self.context()
                            .report_error(n, format!("Invalid numeric literal {e}"));
                    })
                    .unwrap_or(0.0)
            };
        }

        match suffix {
            "i8" => Constant::I8(integer!(i8)),
            "i16" => Constant::I16(integer!(i16)),
            "i32" => Constant::I32(integer!(i32)),
            "i64" => Constant::I64(integer!(i64)),
            "u8" => Constant::U8(integer!(u8)),
            "u16" => Constant::U16(integer!(u16)),
            "u32" => Constant::U32(integer!(u32)),
            "u64" => Constant::U64(integer!(u64)),

            "f32" => Constant::F32(float!(f32)),
            "f64" => Constant::F64(float!(f64)),

            suffix => {
                self.context()
                    .report_error(n, format!("Unknown numeric suffix '{suffix}'"));
                if let Value::Constant(c) = hint.default_value() {
                    c
                } else {
                    Constant::I32(0)
                }
            }
        }
    }
}
