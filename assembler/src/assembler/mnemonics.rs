use crate::assembler::Assembler;
use crate::assembler::translation::{CalculationKind, FormKind};
use crate::context::{Node, NodeId};
use crate::expression::{args, ArgumentsTypeHint, Constant, ExpressionEvaluatorContext, Value, ValueType};
use crate::expression::args::{Immediate, RegReg, StrOpt, U32Opt, U32Power2Opt};
use crate::lex::Token;
use crate::util::IntoStrDelimable;

impl<'a> Assembler<'a> {
    pub(super) fn assemble_mnemonic(&mut self, mnemonic: &'a str, n: NodeId<'a>) {
        match mnemonic {
            "lui" => {}
            "auipc" => {}
            "jal" => {}
            "jalr" => {}

            "beq" => {}
            "bne" => {}
            "blt" => {}
            "bge" => {}
            "bltu" => {}
            "bgeu" => {}

            "lb" => {}
            "lh" => {}
            "lw" => {}
            "lbu" => {}
            "lhu" => {}

            "sb" => {}
            "sh" => {}
            "sw" => {}

            "addi" => {}
            "li" => match self.coerced(n){
                (RegReg(r), Immediate::SignedConstant(c)) => {}
                (RegReg(r), Immediate::UnsignedConstant(c)) => {}
                (RegReg(r), Immediate::Label(c)) => {}
            }
            "ecall" => {}
            "ebreak" => {}

            ".info" => {
                let Node(args, args_node) = self.args(n, ArgumentsTypeHint::None);
                self.context
                    .context
                    .report_info(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".warning" => {
                let Node(args, args_node) = self.args(n, ArgumentsTypeHint::None);
                self.context
                    .context
                    .report_warning(args_node, args.iter().map(|i| i.0).delim(" "))
            }
            ".error" => {
                let Node(args, args_node) = self.args(n, ArgumentsTypeHint::None);
                self.context
                    .context
                    .report_error(args_node, args.iter().map(|i| i.0).delim(" "))
            }

            ".global" => if let Node(StrOpt(Some(label)), node) = self.coerced(n){
                self.context
                    .context
                    .report_warning(node, "not implemented yet");
            }
            ".local" => if let Node(StrOpt(Some(label)), node) = self.coerced(n){
                self.context
                    .context
                    .report_warning(node, "not implemented yet");
            }
            ".weak" => if let Node(StrOpt(Some(label)), node) = self.coerced(n){
                self.context
                    .context
                    .report_warning(node, "not implemented yet");
            }

            ".section" => if let StrOpt(Some(sec)) = self.coerced(n){
                self.context.set_current_section(sec);
            }
            ".org" => if let Node(U32Opt(Some(org)), node) = self.coerced(n){
                if self.context.get_current_section().start.is_some() {
                    self.context
                        .context
                        .report_warning(node, "origin previously set");
                }
                self.context.get_current_section().start = Some(org);
            }
            ".space" => if let U32Opt(Some(size)) = self.coerced(n){
                self.context.add_data(size, 1);
            }
            ".align" => if let U32Power2Opt(Some(align)) = self.coerced(n){
                self.context.add_data(0, align);
            }

            ".data" => {
                for arg in self.args(n, ArgumentsTypeHint::None).0 {
                    let align = arg.0.get_align().unwrap_or(1);
                    match arg.0 {
                        Value::Constant(v) => match v {
                            Constant::I8(v) => {
                                *self.context.add_data_const::<1>(align) = v.to_le_bytes()
                            }
                            Constant::I16(v) => {
                                *self.context.add_data_const::<2>(align) = v.to_le_bytes()
                            }
                            Constant::I32(v) => {
                                *self.context.add_data_const::<4>(align) = v.to_le_bytes()
                            }
                            Constant::I64(v) => {
                                *self.context.add_data_const::<8>(align) = v.to_le_bytes()
                            }
                            Constant::U8(v) => {
                                *self.context.add_data_const::<1>(align) = v.to_le_bytes()
                            }
                            Constant::U16(v) => {
                                *self.context.add_data_const::<2>(align) = v.to_le_bytes()
                            }
                            Constant::U32(v) => {
                                *self.context.add_data_const::<4>(align) = v.to_le_bytes()
                            }
                            Constant::U64(v) => {
                                *self.context.add_data_const::<8>(align) = v.to_le_bytes()
                            }
                            Constant::F32(v) => {
                                *self.context.add_data_const::<4>(align) = v.to_le_bytes()
                            }
                            Constant::F64(v) => {
                                *self.context.add_data_const::<8>(align) = v.to_le_bytes()
                            }
                            Constant::String(v) => self
                                .context
                                .add_data(v.len() as u32, align)
                                .copy_from_slice(v.as_bytes()),
                            Constant::Char(v) => {
                                *self.context.add_data_const::<4>(align) = (v as u32).to_le_bytes()
                            }
                            Constant::Bool(v) => {
                                *self.context.add_data_const::<1>(align) = (v as u8).to_le_bytes()
                            }
                        },
                        Value::Label(l) => {
                            self.context.ins_or_address_reloc(
                                0,
                                l.ident,
                                l.offset,
                                CalculationKind::Absolute,
                                FormKind::Full,
                            );
                        }
                        _ => self
                            .context
                            .context
                            .report_error(arg.1, format!("invalid type {}", arg.0.get_type())),
                    }
                }
            }

            ".string" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::String(v)) = arg.0 {
                        self.context
                            .add_data(v.len() as u32, 1)
                            .copy_from_slice(v.as_bytes())
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected string", arg.0.get_type()),
                        )
                    }
                }
            }
            ".stringz" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::String(v)) = arg.0 {
                        self.context
                            .add_data(v.len() as u32, 1)
                            .copy_from_slice(v.as_bytes());
                        self.context.add_data(1, 1);
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected string", arg.0.get_type()),
                        )
                    }
                }
            }

            ".u8" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U8)).0 {
                    if let Value::Constant(Constant::U8(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<1>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u8", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u16" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U16)).0 {
                    if let Value::Constant(Constant::U16(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<2>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u16", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u32" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U32)).0 {
                    if let Value::Constant(Constant::U32(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<4>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".u64" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::U64)).0 {
                    if let Value::Constant(Constant::U64(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<8>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected u64", arg.0.get_type()),
                        )
                    }
                }
            }

            ".i8" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I8)).0 {
                    if let Value::Constant(Constant::I8(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<1>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i8", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i16" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I16)).0 {
                    if let Value::Constant(Constant::I16(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<2>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i16", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i32" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I32)).0 {
                    if let Value::Constant(Constant::I32(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<4>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".i64" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::I64)).0 {
                    if let Value::Constant(Constant::I64(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<8>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected i64", arg.0.get_type()),
                        )
                    }
                }
            }

            ".f32" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::F32)).0 {
                    if let Value::Constant(Constant::F32(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<4>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected f32", arg.0.get_type()),
                        )
                    }
                }
            }
            ".f64" => {
                for arg in self.args(n, ArgumentsTypeHint::Mono(ValueType::F64)).0 {
                    if let Value::Constant(Constant::F64(v)) = arg.0 {
                        *self
                            .context
                            .add_data_const::<8>(arg.0.get_align().unwrap_or(1)) = v.to_le_bytes();
                    } else {
                        self.context.context.report_error(
                            arg.1,
                            format!("invalid type {} expected f64", arg.0.get_type()),
                        )
                    }
                }
            }

            _ => {
                self.context
                    .context
                    .report_error(n, format!("Unrecognized mnemonic '{mnemonic}'"));

                while !matches!(self.peek(), Some(Node(Token::NewLine, _))) {
                    self.next();
                }
            }
        }
    }
}
