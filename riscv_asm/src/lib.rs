#![allow(unused)]

pub mod args;
pub mod indexed;
pub mod label;
pub mod opcodes;
pub mod reg;
pub mod reloc;

use assembler::assembler::LangCtx;
use assembler::expression::binop::BinOp;
use assembler::simple::{SALState, SimpleAssemblyLanguage, SimpleAssemblyLanguageBase};
use indexed::*;
use opcodes::*;
use reg::*;

use std::convert::Infallible;
use std::marker::PhantomData;
use std::str::FromStr;

use crate::args::{FloatReg, Immediate, RegReg};
use crate::label::{Label, LabelExpr};
use assembler::assembler::{Assembler, lang::AssemblyLanguage};
use assembler::context::{Context, Node, NodeId};
use assembler::expression::args::{AsmStrArg, CoercedArg, LabelArg, U32Arg, U32Pow2Arg};
use assembler::expression::{
    AssemblyRegister, Constant, CustomValue, CustomValueType, EmptyCustomValue, ExprCtx,
    ImplicitCastTo, Indexed, Value, ValueType,
};
use std::fmt::{Display, Formatter};

pub type NodeVal<'a> = assembler::expression::NodeVal<'a, RiscvAssembler<'a>>;
pub type V<'a> = Value<'a, RiscvAssembler<'a>>;

#[derive(Default, Clone)]
pub struct RiscvAssembler<'a> {
    base_state: SALState<'a>,
}
impl<'a> SimpleAssemblyLanguage<'a> for RiscvAssembler<'a> {
    type Reg = Register;
    type Indexed = MemoryIndex<'a>;
    type CustomValue = EmptyCustomValue<Self>;
    type Label = LabelExpr<'a>;
    type AssembledResult = ();

    type Usize = u32;
    type Isize = i32;
    type Uptr = u32;
    type Iptr = i32;

    fn parse_ident(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        Node(ident, node): Node<'a, &'a str>,
        _: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        if let Ok(reg) = Register::from_str(ident) {
            Value::Register(reg)
        } else {
            Value::Label(LabelExpr::new(ident))
        }
    }

    fn assemble_mnemonic(
        &mut self,
        asm: &mut LangCtx<'a, '_, Self>,
        mnemonic: &'a str,
        n: NodeId<'a>,
    ) {
        match mnemonic {
            "lui" => match asm.eval(self).coerced(n).0 {
                (RegReg(r), Immediate::SignedConstant(c)) => {}
                (RegReg(r), Immediate::UnsignedConstant(c)) => {}
                (RegReg(r), Immediate::Label(_)) => {}
            },
            "auipc" => match asm.eval(self).coerced(n).0 {
                (RegReg(r), Immediate::SignedConstant(c)) => {}
                (RegReg(r), Immediate::UnsignedConstant(c)) => {}
                (RegReg(r), Immediate::Label(_)) => {}
            },

            "jal" => self.no_args(asm, n, 0),
            "jalr" => self.no_args(asm, n, 0),

            "beq" => self.no_args(asm, n, 0),
            "bne" => self.no_args(asm, n, 0),
            "blt" => self.no_args(asm, n, 0),
            "bge" => self.no_args(asm, n, 0),
            "bltu" => self.no_args(asm, n, 0),
            "bgeu" => self.no_args(asm, n, 0),

            "lb" => self.no_args(asm, n, 0),
            "lh" => self.no_args(asm, n, 0),
            "lw" => self.no_args(asm, n, 0),
            "lbu" => self.no_args(asm, n, 0),
            "lhu" => self.no_args(asm, n, 0),

            "sb" => self.no_args(asm, n, 0),
            "sh" => self.no_args(asm, n, 0),
            "sw" => self.no_args(asm, n, 0),

            "add" => self.three_int_reg(asm, n, RTypeOpCode::Add),
            "sub" => self.three_int_reg(asm, n, RTypeOpCode::Sub),
            "xor" => self.three_int_reg(asm, n, RTypeOpCode::Xor),
            "or" => self.three_int_reg(asm, n, RTypeOpCode::Or),
            "and" => self.three_int_reg(asm, n, RTypeOpCode::And),
            "slt" => self.three_int_reg(asm, n, RTypeOpCode::Slt),
            "sltu" => self.three_int_reg(asm, n, RTypeOpCode::Sltu),

            "mul" => self.three_int_reg(asm, n, RTypeOpCode::Mul),
            "mulh" => self.three_int_reg(asm, n, RTypeOpCode::Mulh),
            "mulsu" => self.three_int_reg(asm, n, RTypeOpCode::Mulsu),
            "mulu" => self.three_int_reg(asm, n, RTypeOpCode::Mulu),
            "div" => self.three_int_reg(asm, n, RTypeOpCode::Div),
            "divu" => self.three_int_reg(asm, n, RTypeOpCode::Divu),
            "rem" => self.three_int_reg(asm, n, RTypeOpCode::Rem),
            "remu" => self.three_int_reg(asm, n, RTypeOpCode::Remu),

            "ecall" => self.no_args(asm, n, ITypeOpCode::ECall as u32),
            "ebreak" => self.no_args(asm, n, ITypeOpCode::EBreak as u32),

            "nop" => self.no_args(asm, n, ITypeOpCode::Addi as u32),

            "li" | "la" => match asm.eval(self).coerced(n).0 {
                (RegReg(r), Immediate::SignedConstant(c)) if into_12_bit_sign(c) => {
                    self.instruction(
                        asm,
                        ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c as u32),
                        n,
                    );
                }
                (RegReg(r), Immediate::SignedConstant(c)) => {
                    self.instruction(
                        asm,
                        ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c as u32),
                        n,
                    );
                    self.instruction(
                        asm,
                        UTypeOpCode::Lui as u32 | r.rd() | imm_31_12_u(c as u32),
                        n,
                    );
                }
                (RegReg(r), Immediate::UnsignedConstant(c)) if into_12_bit_sign_usg(c) => {
                    self.instruction(asm, ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c), n);
                }
                (RegReg(r), Immediate::UnsignedConstant(c)) => {
                    self.instruction(asm, ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c), n);
                    self.instruction(asm, UTypeOpCode::Lui as u32 | r.rd() | imm_31_12_u(c), n);
                }
                (RegReg(r), Immediate::Label(_)) => {
                    self.instruction(asm, ITypeOpCode::Addi as u32 | r.rd(), n);
                    self.instruction(asm, UTypeOpCode::Lui as u32 | r.rd(), n);
                }
            },
            _ => asm.asm(self).unknown_mnemonic(mnemonic, n),
        }
    }

    fn eval_func(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        func: assembler::expression::FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        match func.func() {
            // "size" => match func.coerced_args(self, ctx) {
            //     Node(Value::Constant(c), _) => {
            //         Value::Constant(Constant::U32(c.get_size()))
            //     }
            //     Node(Value::Label(mut l), n) => {
            //         if l.meta.kind.is_some() {
            //             ctx.context
            //                 .report_error(n, "label relocation kind is already set");
            //         }
            //         l.meta.kind = Some(label::RelocKind::Size);
            //         Value::Label(l)
            //     }
            //     Node(v, n) => {
            //         ctx.context
            //             .report_error(n, format!("cannot get the size for type {}", v.get_type()));
            //         Value::Constant(Constant::U32(1))
            //     }
            // },
            // "align" => match func.coerced_args(self, ctx) {
            //     Node(Value::Constant(c), _) => {
            //         Value::Constant(Constant::U32(c.get_align()))
            //     }
            //     Node(Value::Label(mut l), n) => {
            //         if l.meta.kind.is_some() {
            //             ctx.context
            //                 .report_error(n, "label relocation kind is already set");
            //         }
            //         l.meta.kind = Some(label::RelocKind::Align);
            //         Value::Label(l)
            //     }
            //     Node(v, n) => {
            //         ctx.context.report_error(
            //             n,
            //             format!("cannot get the alignment for type {}", v.get_type()),
            //         );
            //         Value::Constant(Constant::U32(1))
            //     }
            // },
            "pcrel" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    match l.ty {
                        label::LabelExprType::Empty => {}
                        label::LabelExprType::Unspecified(label) => {
                            l.ty = label::LabelExprType::PcRel(label)
                        }
                        label::LabelExprType::Sub(_, _) => ctx
                            .context
                            .report_error(n, "cannot set relocation on label subtraction"),
                        _ => ctx
                            .context
                            .report_error(n, "label relocation kind is already set"),
                    }
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "absolute" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    match l.ty {
                        label::LabelExprType::Empty => {}
                        label::LabelExprType::Unspecified(label) => {
                            l.ty = label::LabelExprType::Absolute(label)
                        }
                        label::LabelExprType::Sub(_, _) => ctx
                            .context
                            .report_error(n, "cannot set relocation on label subtraction"),
                        _ => ctx
                            .context
                            .report_error(n, "label relocation kind is already set"),
                    }
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "hi" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(label::RelocPattern::High);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "lo" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(label::RelocPattern::Low);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "u8" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(label::RelocPattern::U8);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "u16" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(label::RelocPattern::U16);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "u32" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(label::RelocPattern::U32);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            "u64" => {
                if let Node(LabelArg(Some(mut l)), n) = func.coerced_args(self, ctx).0 {
                    if l.pattern.is_some() {
                        ctx.context
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.pattern = Some(label::RelocPattern::U64);
                    Value::Label(l)
                } else {
                    Value::Label(LabelExpr::default())
                }
            }
            _ => ctx.eval(self).func_base(func, hint),
        }
    }

    fn eval_index(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeId<'a>,
        lhs: Option<NodeVal<'a>>,
        opening: NodeId<'a>,
        rhs: Option<NodeVal<'a>>,
        closing: NodeId<'a>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        let (lhs, rhs) = match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => (lhs, rhs),
            (lhs, rhs) => {
                return ctx
                    .eval(self)
                    .index_base(node, lhs, opening, rhs, closing, hint);
            }
        };
        match (lhs.0, rhs.0) {
            (Value::Register(r), Value::Constant(Constant::I32(i))) => {
                Value::Indexed(MemoryIndex::RegisterOffset(r, i))
            }
            (Value::Constant(Constant::I32(i)), Value::Register(r)) => {
                Value::Indexed(MemoryIndex::RegisterOffset(r, i))
            }
            (
                Value::Indexed(MemoryIndex::RegisterOffset(r, o)),
                Value::Constant(Constant::I32(i)),
            ) => Value::Indexed(MemoryIndex::RegisterOffset(r, o.wrapping_add(i))),
            (
                Value::Constant(Constant::I32(i)),
                Value::Indexed(MemoryIndex::RegisterOffset(r, o)),
            ) => Value::Indexed(MemoryIndex::RegisterOffset(r, o.wrapping_add(i))),

            (Value::Label(l), Value::Constant(Constant::I32(i))) => Value::Label(l.offset(i)),
            (Value::Constant(Constant::I32(i)), Value::Label(l)) => Value::Label(l.offset(i)),
            (Value::Label(l), Value::Register(r)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }
            (Value::Register(r), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }

            (Value::Label(l), Value::Indexed(MemoryIndex::RegisterOffset(r, i))) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l.offset(i)))
            }
            (Value::Indexed(MemoryIndex::RegisterOffset(r, i)), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l.offset(i)))
            }
            _ => ctx
                .eval(self)
                .index_base(node, Some(lhs), opening, Some(rhs), closing, hint),
        }
    }

    fn eval_binop(
        &mut self,
        ctx: &mut ExprCtx<'a, '_, Self>,
        node: NodeId<'a>,
        lhs: assembler::expression::NodeVal<'a, Self>,
        op: Node<'a, assembler::expression::binop::BinOp>,
        rhs: assembler::expression::NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        let lbl_config = ctx.context.config().implicit_cast_label_offset;
        match (op.0, lhs, rhs) {
            (BinOp::Add, Node(Value::Constant(c), cn), Node(Value::Indexed(idx), _))
            | (BinOp::Add, Node(Value::Indexed(idx), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                match idx {
                    MemoryIndex::LabelRegisterOffset(register, label) => {
                        Value::Indexed(MemoryIndex::LabelRegisterOffset(
                            register,
                            label.offset(
                                c.cast_with(cn, ctx.context, lbl_config).unwrap_or_default(),
                            ),
                        ))
                    }
                    MemoryIndex::RegisterOffset(register, offset) => {
                        Value::Indexed(MemoryIndex::RegisterOffset(
                            register,
                            offset.wrapping_add(
                                c.cast_with(cn, ctx.context, lbl_config).unwrap_or_default(),
                            ),
                        ))
                    }
                }
            }

            (BinOp::Sub, Node(Value::Indexed(idx), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                match idx {
                    MemoryIndex::LabelRegisterOffset(register, label) => {
                        Value::Indexed(MemoryIndex::LabelRegisterOffset(
                            register,
                            label.offset(
                                c.cast_with(cn, ctx.context, lbl_config)
                                    .unwrap_or(0i32)
                                    .wrapping_neg(),
                            ),
                        ))
                    }
                    MemoryIndex::RegisterOffset(register, offset) => {
                        Value::Indexed(MemoryIndex::RegisterOffset(
                            register,
                            offset.wrapping_add(
                                c.cast_with(cn, ctx.context, lbl_config)
                                    .unwrap_or(0i32)
                                    .wrapping_neg(),
                            ),
                        ))
                    }
                }
            }

            (BinOp::Add, Node(Value::Register(reg), _), Node(Value::Constant(c), cn))
            | (BinOp::Add, Node(Value::Constant(c), cn), Node(Value::Register(reg), _))
                if c.is_integer() =>
            {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    reg,
                    c.cast_with(cn, ctx.context, lbl_config).unwrap_or(0i32),
                ))
            }

            (BinOp::Sub, Node(Value::Register(reg), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    reg,
                    c.cast_with(cn, ctx.context, lbl_config)
                        .unwrap_or(0i32)
                        .wrapping_neg(),
                ))
            }

            (BinOp::Sub, Node(Value::Label(lhs), _), Node(Value::Label(rhs), _)) => {
                if lhs.pattern.is_some() || rhs.pattern.is_some() {
                    ctx.context.report_error(
                        node,
                        "both lhs and rhs must not have a specified representation",
                    );
                    return Value::Label(LabelExpr::default());
                }
                match (lhs.ty, rhs.ty) {
                    (
                        label::LabelExprType::Unspecified(llhs),
                        label::LabelExprType::Unspecified(lrhs),
                    ) => {
                        return Value::Label(LabelExpr {
                            ty: label::LabelExprType::Sub(llhs, lrhs),
                            offset: lhs.offset.wrapping_add(rhs.offset),
                            pattern: None,
                        });
                    }
                    (label::LabelExprType::Sub(_, _), _)
                    | (_, label::LabelExprType::Sub(_, _))
                    | (label::LabelExprType::Sub(_, _), label::LabelExprType::Sub(_, _)) => {
                        ctx.context
                            .report_error(node, "can only calculate different between two labels");
                    }
                    _ => {
                        ctx.context.report_error(
                            node,
                            "both lhs and rhs must not have a specified relocation kind",
                        );
                    }
                };
                Value::Label(LabelExpr::default())
            }

            (BinOp::Add, Node(Value::Label(l), _), Node(Value::Constant(c), cn))
            | (BinOp::Add, Node(Value::Constant(c), cn), Node(Value::Label(l), _))
                if c.is_integer() =>
            {
                Value::Label(l.offset(c.cast_with(cn, ctx.context, lbl_config).unwrap_or(0i32)))
            }

            (BinOp::Sub, Node(Value::Label(l), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                Value::Label(
                    l.offset(
                        c.cast_with(cn, ctx.context, lbl_config)
                            .unwrap_or(0i32)
                            .wrapping_neg(),
                    ),
                )
            }

            _ => ctx.eval(self).binop_base(node, lhs, op, rhs, hint),
        }
    }

    fn finish(&mut self, ctx: LangCtx<'a, '_, Self>) -> Self::AssembledResult {
        // todo!()
    }

    fn encounter_label(&mut self, ctx: &mut LangCtx<'a, '_, Self>, label: &'a str, n: NodeId<'a>) {
        // todo!()
    }

    fn add_value_data(
        &mut self,
        ctx: &mut LangCtx<'a, '_, Self>,
        value: Value<'a, Self>,
        n: NodeId<'a>,
    ) {
        match value {
            Value::Constant(constant) => self.add_constant_data(ctx, constant, n),
            _ => ctx
                .context
                .report_error(n, format!("cannot use '{}' as data", value.get_type())),
        }
    }

    fn state_mut(&mut self) -> &mut assembler::simple::SALState<'a> {
        &mut self.base_state
    }

    fn state(&self) -> &assembler::simple::SALState<'a> {
        &self.base_state
    }
}

impl<'a> RiscvAssembler<'a> {
    fn instruction(&mut self, ctx: &mut LangCtx<'a, '_, Self>, ins: u32, node: NodeId<'a>) {
        self.add_data(ctx, &ins.to_le_bytes(), 4, node);
        // *asm.state.add_data_const::<4>(4) = ins.to_le_bytes();
    }

    fn three_int_reg(
        &mut self,
        asm: &mut LangCtx<'a, '_, Self>,
        node: NodeId<'a>,
        ins: RTypeOpCode,
    ) {
        let (RegReg(rd), RegReg(rs1), RegReg(rs2)) = asm.eval(self).coerced(node).0;
        self.instruction(asm, ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2(), node);
    }

    fn two_int_reg(&mut self, asm: &mut LangCtx<'a, '_, Self>, node: NodeId<'a>, ins: RTypeOpCode) {
        let (RegReg(rd), RegReg(rs1)) = asm.eval(self).coerced(node).0;
        self.instruction(asm, ins as u32 | rd.rd() | rs1.rs1(), node);
    }

    fn float_reg_only_3(
        &mut self,
        asm: &mut LangCtx<'a, '_, Self>,
        node: NodeId<'a>,
        ins: RTypeOpCode,
    ) {
        let (FloatReg(rd), FloatReg(rs1), FloatReg(rs2)) = asm.eval(self).coerced(node).0;
        self.instruction(asm, ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2(), node);
    }

    fn float_reg_only_4(
        &mut self,
        asm: &mut LangCtx<'a, '_, Self>,
        node: NodeId<'a>,
        ins: RTypeOpCode,
    ) {
        let (FloatReg(rd), FloatReg(rs1), FloatReg(rs2), FloatReg(rs3)) =
            asm.eval(self).coerced(node).0;
        self.instruction(
            asm,
            ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2() | rs3.rs3(),
            node,
        );
    }

    fn no_args(&mut self, asm: &mut LangCtx<'a, '_, Self>, node: NodeId<'a>, ins: u32) {
        let _: () = asm.eval(self).coerced(node).0;
        self.instruction(asm, ins, node);
    }
}
