use std::fmt::Write;

use crate::{
    assembler::lang::AssemblyLanguage,
    context::Node,
    expression::{
        Constant, ExpressionEvaluator, ExpressionEvaluatorContext, FuncParamParser, Value,
        ValueType, args::StrOpt,
    },
};

impl<'a, 'b, L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>
    ExpressionEvaluator<'a, 'b, L, T>
{
    pub fn func_base(
        &mut self,
        func: FuncParamParser<'a, '_>,
        hint: ValueType<'a, L>,
    ) -> Value<'a, L> {
        match func.func() {
            "format" => {
                if let Node((StrOpt::Val(Some(format)), v), n) =
                    func.coerced_args::<(_, Vec<Value<'a, L>>), _, _>(self.0)
                {
                    let expected = format.matches('%').count();
                    if expected != v.len() {
                        self.context().report_error(
                            n,
                            format!(
                                "wrong number of arguments provided expected {} found {}",
                                expected,
                                v.len()
                            ),
                        )
                    }
                    let mut result = String::new();
                    let mut iter = v.into_iter();
                    for part in format.split('%') {
                        result
                            .write_fmt(format_args!(
                                "{}{}",
                                part,
                                iter.next().unwrap_or(Value::Constant(Constant::String("")))
                            ))
                            .unwrap();
                    }

                    Value::Constant(Constant::String(self.context().alloc_str(result)))
                } else {
                    Value::Constant(Constant::String(""))
                }
            }
            _ => self.invalid_func(func, hint),
        }
    }

    pub fn invalid_func(
        &mut self,
        func: FuncParamParser<'a, '_>,
        hint: ValueType<'a, L>,
    ) -> Value<'a, L> {
        let func_name = func.func.0;
        let node = func.args(self.0).1;
        self.context()
            .report_error(node, format!("Unknown function {}", func_name));
        hint.default_value()
    }
}
