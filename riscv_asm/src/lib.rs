#![allow(unused)]

pub mod args;
pub mod indexed;
pub mod label;
pub mod opcodes;
pub mod reg;

use assembler::expression::binop::BinOp;
use indexed::*;
use opcodes::*;
use reg::*;

use std::convert::Infallible;
use std::marker::PhantomData;
use std::str::FromStr;

use crate::args::{FloatReg, Immediate, RegReg};
use crate::label::Label;
use assembler::assembler::{Assembler, lang::AssemblyLanguage};
use assembler::context::{Context, Node, NodeId};
use assembler::expression::args::{CoercedArg, LabelOpt};
use assembler::expression::{
    AssemblyRegister, Constant, CustomValue, CustomValueType, ExpressionEvaluatorContext,
    ImplicitCastTo, Indexed, Value, ValueType,
};
use std::fmt::{Display, Formatter};

pub type NodeVal<'a> = assembler::expression::NodeVal<'a, RiscvAssembler>;

#[derive(Default, Clone)]
pub struct RiscvAssembler;
impl<'a> AssemblyLanguage<'a> for RiscvAssembler {
    type Reg = Register;
    type Indexed = MemoryIndex<'a>;
    type CustomValue = Infallible;
    type Label = Label<'a>;

    fn parse_ident(
        _: &mut impl ExpressionEvaluatorContext<'a, Self>,
        Node(ident, node): Node<'a, &'a str>,
        _: ValueType<'a, RiscvAssembler>,
    ) -> Value<'a, Self> {
        if let Ok(reg) = Register::from_str(ident) {
            Value::Register(reg)
        } else {
            Value::Label(Label::new(ident))
        }
    }

    fn assemble_mnemonic(asm: &mut Assembler<'a, '_, Self>, mnemonic: &'a str, n: NodeId<'a>) {
        match mnemonic {
            "lui" => match asm.coerced(n).0 {
                (RegReg(r), Immediate::SignedConstant(c)) => {}
                (RegReg(r), Immediate::UnsignedConstant(c)) => {}
                (RegReg(r), Immediate::Label(_)) => {}
            },
            "auipc" => match asm.coerced(n).0 {
                (RegReg(r), Immediate::SignedConstant(c)) => {}
                (RegReg(r), Immediate::UnsignedConstant(c)) => {}
                (RegReg(r), Immediate::Label(_)) => {}
            },

            "jal" => Self::no_args(asm, n, 0),
            "jalr" => Self::no_args(asm, n, 0),

            "beq" => Self::no_args(asm, n, 0),
            "bne" => Self::no_args(asm, n, 0),
            "blt" => Self::no_args(asm, n, 0),
            "bge" => Self::no_args(asm, n, 0),
            "bltu" => Self::no_args(asm, n, 0),
            "bgeu" => Self::no_args(asm, n, 0),

            "lb" => Self::no_args(asm, n, 0),
            "lh" => Self::no_args(asm, n, 0),
            "lw" => Self::no_args(asm, n, 0),
            "lbu" => Self::no_args(asm, n, 0),
            "lhu" => Self::no_args(asm, n, 0),

            "sb" => Self::no_args(asm, n, 0),
            "sh" => Self::no_args(asm, n, 0),
            "sw" => Self::no_args(asm, n, 0),

            "add" => Self::three_int_reg(asm, n, RTypeOpCode::Add),
            "sub" => Self::three_int_reg(asm, n, RTypeOpCode::Sub),
            "xor" => Self::three_int_reg(asm, n, RTypeOpCode::Xor),
            "or" => Self::three_int_reg(asm, n, RTypeOpCode::Or),
            "and" => Self::three_int_reg(asm, n, RTypeOpCode::And),
            "slt" => Self::three_int_reg(asm, n, RTypeOpCode::Slt),
            "sltu" => Self::three_int_reg(asm, n, RTypeOpCode::Sltu),

            "mul" => Self::three_int_reg(asm, n, RTypeOpCode::Mul),
            "mulh" => Self::three_int_reg(asm, n, RTypeOpCode::Mulh),
            "mulsu" => Self::three_int_reg(asm, n, RTypeOpCode::Mulsu),
            "mulu" => Self::three_int_reg(asm, n, RTypeOpCode::Mulu),
            "div" => Self::three_int_reg(asm, n, RTypeOpCode::Div),
            "divu" => Self::three_int_reg(asm, n, RTypeOpCode::Divu),
            "rem" => Self::three_int_reg(asm, n, RTypeOpCode::Rem),
            "remu" => Self::three_int_reg(asm, n, RTypeOpCode::Remu),

            "nop" => Self::no_args(asm, n, ITypeOpCode::Addi as u32),

            "li" | "la  " => match asm.coerced(n).0 {
                (RegReg(r), Immediate::SignedConstant(c)) if into_12_bit_sign(c) => {
                    Self::instruction(
                        asm,
                        ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c as u32),
                    );
                }
                (RegReg(r), Immediate::SignedConstant(c)) => {
                    Self::instruction(
                        asm,
                        ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c as u32),
                    );
                    Self::instruction(
                        asm,
                        UTypeOpCode::Lui as u32 | r.rd() | imm_31_12_u(c as u32),
                    );
                }
                (RegReg(r), Immediate::UnsignedConstant(c)) if into_12_bit_sign_usg(c) => {
                    Self::instruction(asm, ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c));
                }
                (RegReg(r), Immediate::UnsignedConstant(c)) => {
                    Self::instruction(asm, ITypeOpCode::Addi as u32 | r.rd() | imm_11_0_s(c));
                    Self::instruction(asm, UTypeOpCode::Lui as u32 | r.rd() | imm_31_12_u(c));
                }
                (RegReg(r), Immediate::Label(_)) => {
                    Self::instruction(asm, ITypeOpCode::Addi as u32 | r.rd());
                    Self::instruction(asm, UTypeOpCode::Lui as u32 | r.rd());
                }
            },
            _ => asm.assemble_mnemonic_default(Node(mnemonic, n)),
        }
    }

    fn add_label_as_data(asm: &mut Assembler<'a, '_, Self>, l: Self::Label, node: NodeId<'a>) {
        // asm.state.ins_or_address_reloc(
        //     0,
        //     l.ident,
        //     l.offset,
        //     CalculationKind::Absolute,
        //     FormKind::Full,
        // );
        todo!()
    }

    fn eval_func(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        func: assembler::expression::FuncParamParser<'a, '_>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        match func.func() {
            "size" => match func.coerced_args(ctx) {
                Node(Value::Constant(c), _) => {
                    Value::Constant(Constant::U32(c.get_size().unwrap_or(1)))
                }
                Node(Value::Label(mut l), n) => {
                    if l.meta.kind.is_some() {
                        ctx.context()
                            .report_error(n, "label relocation kind is already set");
                    }
                    l.meta.kind = Some(label::RelocKind::Size);
                    Value::Label(l)
                }
                Node(v, n) => {
                    ctx.context()
                        .report_error(n, format!("cannot get the size for type {}", v.get_type()));
                    Value::Constant(Constant::U32(1))
                }
            },
            "align" => match func.coerced_args(ctx) {
                Node(Value::Constant(c), _) => {
                    Value::Constant(Constant::U32(c.get_align().unwrap_or(1)))
                }
                Node(Value::Label(mut l), n) => {
                    if l.meta.kind.is_some() {
                        ctx.context()
                            .report_error(n, "label relocation kind is already set");
                    }
                    l.meta.kind = Some(label::RelocKind::Align);
                    Value::Label(l)
                }
                Node(v, n) => {
                    ctx.context().report_error(
                        n,
                        format!("cannot get the alignment for type {}", v.get_type()),
                    );
                    Value::Constant(Constant::U32(1))
                }
            },
            "pcrel" => {
                if let Node(LabelOpt(Some(mut l)), n) = func.coerced_args(ctx).0 {
                    if l.meta.kind.is_some() {
                        ctx.context()
                            .report_error(n, "label relocation kind is already set");
                    }
                    l.meta.kind = Some(label::RelocKind::PcRel);
                    Value::Label(l)
                } else {
                    Value::Label(Label::default())
                }
            }
            "absolute" => {
                if let Node(LabelOpt(Some(mut l)), n) = func.coerced_args(ctx).0 {
                    if l.meta.kind.is_some() {
                        ctx.context()
                            .report_error(n, "label relocation kind is already set");
                    }
                    l.meta.kind = Some(label::RelocKind::Absolute);
                    Value::Label(l)
                } else {
                    Value::Label(Label::default())
                }
            }
            "hi" => {
                if let Node(LabelOpt(Some(mut l)), n) = func.coerced_args(ctx).0 {
                    if l.meta.pattern.is_some() {
                        ctx.context()
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.meta.pattern = Some(label::RelocPattern::High);
                    Value::Label(l)
                } else {
                    Value::Label(Label::default())
                }
            }
            "lo" => {
                if let Node(LabelOpt(Some(mut l)), n) = func.coerced_args(ctx).0 {
                    if l.meta.pattern.is_some() {
                        ctx.context()
                            .report_error(n, "label pattern kind is already set");
                    }
                    l.meta.pattern = Some(label::RelocPattern::Low);
                    Value::Label(l)
                } else {
                    Value::Label(Label::default())
                }
            }
            _ => ctx.eval().func_base(func, hint),
        }
    }

    fn eval_index(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        node: NodeId<'a>,
        lhs: assembler::expression::NodeVal<'a, Self>,
        opening: NodeId<'a>,
        rhs: assembler::expression::NodeVal<'a, Self>,
        closing: NodeId<'a>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        match (rhs.0, lhs.0) {
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

            (Value::Label(l), Value::Constant(Constant::I32(i))) => Value::Label(Label {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Constant(Constant::I32(i)), Value::Label(l)) => Value::Label(Label {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Label(l), Value::Register(r)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }
            (Value::Register(r), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(r, l))
            }

            (Value::Label(l), Value::Indexed(MemoryIndex::RegisterOffset(r, i))) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(
                    r,
                    Label {
                        ident: l.ident,
                        offset: l.offset.wrapping_add(i),
                        meta: l.meta,
                    },
                ))
            }
            (Value::Indexed(MemoryIndex::RegisterOffset(r, i)), Value::Label(l)) => {
                Value::Indexed(MemoryIndex::LabelRegisterOffset(
                    r,
                    Label {
                        ident: l.ident,
                        offset: l.offset.wrapping_add(i),
                        meta: l.meta,
                    },
                ))
            }
            _ => ctx
                .eval()
                .index_base(node, lhs, opening, rhs, closing, hint),
        }
    }

    fn eval_binop(
        ctx: &mut impl ExpressionEvaluatorContext<'a, Self>,
        node: NodeId<'a>,
        lhs: assembler::expression::NodeVal<'a, Self>,
        op: Node<'a, assembler::expression::binop::BinOp>,
        rhs: assembler::expression::NodeVal<'a, Self>,
        hint: ValueType<'a, Self>,
    ) -> Value<'a, Self> {
        let lbl_config = ctx.context().config().implicit_cast_label_offset;
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
                                c.cast_with(cn, ctx.context(), lbl_config)
                                    .unwrap_or_default(),
                            ),
                        ))
                    }
                    MemoryIndex::RegisterOffset(register, offset) => {
                        Value::Indexed(MemoryIndex::RegisterOffset(
                            register,
                            offset.wrapping_add(
                                c.cast_with(cn, ctx.context(), lbl_config)
                                    .unwrap_or_default(),
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
                                c.cast_with(cn, ctx.context(), lbl_config)
                                    .unwrap_or(0i32)
                                    .wrapping_neg(),
                            ),
                        ))
                    }
                    MemoryIndex::RegisterOffset(register, offset) => {
                        Value::Indexed(MemoryIndex::RegisterOffset(
                            register,
                            offset.wrapping_add(
                                c.cast_with(cn, ctx.context(), lbl_config)
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
                    c.cast_with(cn, ctx.context(), lbl_config).unwrap_or(0i32),
                ))
            }

            (BinOp::Sub, Node(Value::Register(reg), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                Value::Indexed(MemoryIndex::RegisterOffset(
                    reg,
                    c.cast_with(cn, ctx.context(), lbl_config)
                        .unwrap_or(0i32)
                        .wrapping_neg(),
                ))
            }

            (BinOp::Add, Node(Value::Label(l), _), Node(Value::Constant(c), cn))
            | (BinOp::Add, Node(Value::Constant(c), cn), Node(Value::Label(l), _))
                if c.is_integer() =>
            {
                Value::Label(l.offset(c.cast_with(cn, ctx.context(), lbl_config).unwrap_or(0i32)))
            }

            (BinOp::Sub, Node(Value::Label(l), _), Node(Value::Constant(c), cn))
                if c.is_integer() =>
            {
                Value::Label(
                    l.offset(
                        c.cast_with(cn, ctx.context(), lbl_config)
                            .unwrap_or(0i32)
                            .wrapping_neg(),
                    ),
                )
            }

            _ => ctx.eval().binop_base(node, lhs, op, rhs, hint),
        }
    }
}

impl RiscvAssembler {
    fn instruction<'a>(asm: &mut Assembler<'a, '_, Self>, ins: u32) {
        *asm.state.add_data_const::<4>(4) = ins.to_le_bytes();
    }

    fn three_int_reg<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: RTypeOpCode) {
        let (RegReg(rd), RegReg(rs1), RegReg(rs2)) = asm.coerced(n).0;
        Self::instruction(asm, ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2());
    }

    fn two_int_reg<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: RTypeOpCode) {
        let (RegReg(rd), RegReg(rs1)) = asm.coerced(n).0;
        Self::instruction(asm, ins as u32 | rd.rd() | rs1.rs1());
    }

    fn float_reg_only_3<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: RTypeOpCode) {
        let (FloatReg(rd), FloatReg(rs1), FloatReg(rs2)) = asm.coerced(n).0;
        Self::instruction(asm, ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2());
    }

    fn float_reg_only_4<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: RTypeOpCode) {
        let (FloatReg(rd), FloatReg(rs1), FloatReg(rs2), FloatReg(rs3)) = asm.coerced(n).0;
        Self::instruction(
            asm,
            ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2() | rs3.rs3(),
        );
    }

    fn no_args<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: u32) {
        let _: () = asm.coerced(n).0;
        Self::instruction(asm, ins);
    }
}
