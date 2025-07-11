#![allow(unused)]

pub mod args;
pub mod indexed;
pub mod label;
pub mod opcodes;
pub mod reg;

use indexed::*;
use opcodes::*;
use reg::*;

use std::convert::Infallible;
use std::marker::PhantomData;
use std::str::FromStr;

use crate::args::{FloatReg, RegReg};
use crate::label::Label;
use assembler::assembler::{Assembler, AssemblyLanguage};
use assembler::context::{Context, Node, NodeId};
use assembler::expression::args::{CoercedArg, Immediate};
use assembler::expression::{
    AssemblyRegister, Constant, CustomValue, CustomValueType, ExpressionEvaluatorContext, Indexed,
    Value, ValueType,
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

    fn parse_ident_assembly(
        _: &mut Assembler<'a, '_, Self>,
        ident: &'a str,
        node: NodeId<'a>,
        _: ValueType<'a, RiscvAssembler>,
    ) -> Node<'a, Value<'a, Self>> {
        if let Ok(reg) = Register::from_str(ident) {
            Node(Value::Register(reg), node)
        } else {
            Node(Value::Label(Label::new(ident)), node)
        }
    }

    fn assemble_mnemonic(asm: &mut Assembler<'a, '_, Self>, mnemonic: &'a str, n: NodeId<'a>) {
        match mnemonic {
            "lui" => match asm.coerced(n) {
                (RegReg(r), Immediate::SignedConstant(c)) => {}
                (RegReg(r), Immediate::UnsignedConstant(c)) => {}
                (RegReg(r), Immediate::Label(_)) => {}
            },
            "auipc" => match asm.coerced(n) {
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

            "add" => Self::reg_reg_only(asm, n, RTypeOpCode::Add),
            "sub" => Self::reg_reg_only(asm, n, RTypeOpCode::Sub),
            "xor" => Self::reg_reg_only(asm, n, RTypeOpCode::Xor),
            "or" => Self::reg_reg_only(asm, n, RTypeOpCode::Or),
            "and" => Self::reg_reg_only(asm, n, RTypeOpCode::And),
            "slt" => Self::reg_reg_only(asm, n, RTypeOpCode::Slt),
            "sltu" => Self::reg_reg_only(asm, n, RTypeOpCode::Sltu),

            "mul" => Self::reg_reg_only(asm, n, RTypeOpCode::Mul),
            "mulh" => Self::reg_reg_only(asm, n, RTypeOpCode::Mulh),
            "mulsu" => Self::reg_reg_only(asm, n, RTypeOpCode::Mulsu),
            "mulu" => Self::reg_reg_only(asm, n, RTypeOpCode::Mulu),
            "div" => Self::reg_reg_only(asm, n, RTypeOpCode::Div),
            "divu" => Self::reg_reg_only(asm, n, RTypeOpCode::Divu),
            "rem" => Self::reg_reg_only(asm, n, RTypeOpCode::Rem),
            "remu" => Self::reg_reg_only(asm, n, RTypeOpCode::Remu),

            "li" => match asm.coerced(n) {
                // (RegReg(r), Immediate::SignedConstant(c)) if c <= i16::MAX as i32 && c >= i16::MIN as i32 => {
                //     Self::instruction(ITypeOpCode::Addi as u32 | r.rd() | instructions::imm_11_0_s(asm, c as u32))
                // }
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
                // (RegReg(r), Immediate::UnsignedConstant(c)) if c <= i16::MAX as u32 => {
                //     Self::instruction(ITypeOpCode::Addi as u32 | r.rd() | instructions::imm_11_0_s(asm, c))
                // }
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
}

impl RiscvAssembler {
    fn instruction<'a>(asm: &mut Assembler<'a, '_, Self>, ins: u32) {
        *asm.state.add_data_const::<4>(4) = ins.to_le_bytes();
    }

    fn reg_reg_only<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: RTypeOpCode) {
        let (RegReg(rd), RegReg(rs1), RegReg(rs2)) = asm.coerced(n);
        Self::instruction(asm, ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2());
    }

    fn float_reg_only_3<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: RTypeOpCode) {
        let (FloatReg(rd), FloatReg(rs1), FloatReg(rs2)) = asm.coerced(n);
        Self::instruction(asm, ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2());
    }

    fn float_reg_only_4<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: RTypeOpCode) {
        let (FloatReg(rd), FloatReg(rs1), FloatReg(rs2), FloatReg(rs3)) = asm.coerced(n);
        Self::instruction(
            asm,
            ins as u32 | rd.rd() | rs1.rs1() | rs2.rs2() | rs3.rs3(),
        );
    }

    fn no_args<'a>(asm: &mut Assembler<'a, '_, Self>, n: NodeId<'a>, ins: u32) {
        let _: () = asm.coerced(n);
        Self::instruction(asm, ins);
    }
}
