use std::fmt::{Debug, Display, Formatter};
use std::ops::Index;

use crate::assembler::AssemblyLanguage;
use crate::context::NodeId;
use crate::expression::{ExpressionEvaluatorContext, NodeVal};


// pub type ValueType<'a, L: AssemblyLanguage<'a>> = ValueTypeI<L::CustomValueType>; 

#[derive(Debug, Eq, Clone)]
pub enum ValueType<'a, L: AssemblyLanguage<'a>> {
    Any,

    String,

    Indexed,
    Register,

    Label,

    I8,
    I16,
    I32,
    I64,

    U8,
    U16,
    U32,
    U64,

    F32,
    F64,

    Bool,
    Char,
    Custom(L::CustomValueType),
}

impl<'a, L> core::cmp::PartialEq for ValueType<'a, L>
where
    L: AssemblyLanguage<'a>,
    L::CustomValueType: core::cmp::PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueType::Any, ValueType::Any) => true,
            (ValueType::String, ValueType::String) => true,
            (ValueType::Indexed, ValueType::Indexed) => true,
            (ValueType::Register, ValueType::Register) => true,
            (ValueType::Label, ValueType::Label) => true,
            (ValueType::I8, ValueType::I8) => true,
            (ValueType::I16, ValueType::I16) => true,
            (ValueType::I32, ValueType::I32) => true,
            (ValueType::I64, ValueType::I64) => true,
            (ValueType::U8, ValueType::U8) => true,
            (ValueType::U16, ValueType::U16) => true,
            (ValueType::U32, ValueType::U32) => true,
            (ValueType::U64, ValueType::U64) => true,
            (ValueType::F32, ValueType::F32) => true,
            (ValueType::F64, ValueType::F64) => true,
            (ValueType::Bool, ValueType::Bool) => true,
            (ValueType::Char, ValueType::Char) => true,
            (ValueType::Custom(f0_self), ValueType::Custom(f0_other)) => f0_self.eq(f0_other),
            _unused => false,
        }
    }
}


impl<'a, L> Copy for ValueType<'a, L>
where
    L: AssemblyLanguage<'a>,
    L::CustomValueType: Copy,
{}

impl<'a, L: AssemblyLanguage<'a>> Display for ValueType<'a, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Any => write!(f, "any"),
            ValueType::String => write!(f, "string"),
            ValueType::Indexed => write!(f, "indexed"),
            ValueType::Register => write!(f, "register"),
            ValueType::Label => write!(f, "label"),
            ValueType::I8 => write!(f, "i8"),
            ValueType::I16 => write!(f, "i16"),
            ValueType::I32 => write!(f, "i32"),
            ValueType::I64 => write!(f, "i64"),
            ValueType::U8 => write!(f, "u8"),
            ValueType::U16 => write!(f, "u16"),
            ValueType::U32 => write!(f, "u32"),
            ValueType::U64 => write!(f, "u64"),
            ValueType::F32 => write!(f, "f32"),
            ValueType::F64 => write!(f, "f64"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Char => write!(f, "char"),
            ValueType::Custom(custom) => write!(f, "{custom}"),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> ValueType<'a, L> {
    pub fn default_value(&self) -> Value<'a, L> {
        match self {
            ValueType::Any => Value::Constant(Constant::I32(0)),
            ValueType::String => Value::Constant(Constant::String("")),
            ValueType::Indexed => Value::Indexed(L::Indexed::default()),
            ValueType::Register => Value::Register(L::RegType::default()),
            ValueType::Label => Value::Label(LabelUse {
                ident: "",
                offset: 0,
                meta: LabelMeta::Unset,
            }),
            ValueType::I8 => Value::Constant(Constant::I8(0)),
            ValueType::I16 => Value::Constant(Constant::I16(0)),
            ValueType::I32 => Value::Constant(Constant::I32(0)),
            ValueType::I64 => Value::Constant(Constant::I16(0)),
            ValueType::U8 => Value::Constant(Constant::U8(0)),
            ValueType::U16 => Value::Constant(Constant::U16(0)),
            ValueType::U32 => Value::Constant(Constant::U32(0)),
            ValueType::U64 => Value::Constant(Constant::U64(0)),
            ValueType::F32 => Value::Constant(Constant::F32(0.0)),
            ValueType::F64 => Value::Constant(Constant::F64(0.0)),
            ValueType::Bool => Value::Constant(Constant::Bool(false)),
            ValueType::Char => Value::Constant(Constant::Char('\0')),
            ValueType::Custom(c) => Value::Custom(c.default_value())
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum LabelMeta {
    PcRel,
    Absolute,
    Size,
    Align,
    Unset,
}

impl LabelMeta {
    pub fn is_unset(&self) -> bool {
        matches!(self, LabelMeta::Unset)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct LabelUse<'a> {
    pub ident: &'a str,
    pub offset: i32,
    pub meta: LabelMeta,
}

impl<'a> LabelUse<'a> {
    pub fn new(ident: &'a str) -> LabelUse<'a> {
        LabelUse {
            ident,
            offset: 0,
            meta: LabelMeta::Unset,
        }
    }
}

impl<'a> Display for LabelUse<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.meta {
            LabelMeta::PcRel => write!(f, "pc_rel(")?,
            LabelMeta::Absolute => write!(f, "absolute(")?,
            LabelMeta::Size => write!(f, "size(")?,
            LabelMeta::Align => write!(f, "align(")?,
            LabelMeta::Unset => {}
        }
        write!(f, "{}", self.ident)?;
        if self.offset != 0 {
            write!(f, "+{}", self.offset)?;
        }
        if !self.meta.is_unset() {
            write!(f, ")")?;
        }
        Ok(())
    }
}

pub trait CustomValue<'a, L: AssemblyLanguage<'a>>: Debug + Clone + Copy + PartialEq + Display + Default + Sized{
    fn get_align(&self) -> Option<u32>;
    fn get_size(&self) -> Option<u32>;
    fn get_type(&self) -> L::CustomValueType;
}
pub trait CustomValueType<'a, L: AssemblyLanguage<'a>>: Debug + Clone + Copy + PartialEq + Eq + Display + Default + Sized + 'static{
    fn default_value(&self) -> L::CustomValue;
}


pub trait AssemblyRegister: Debug + Clone + Copy + PartialEq + Display + Default + Sized {}
pub trait Indexed<'a, L: AssemblyLanguage<'a>>:
    Debug + Clone + Copy + PartialEq + Display + Default + Sized
{
    fn from_indexed(
        ctx: &mut impl ExpressionEvaluatorContext<'a, L>,
        node: NodeId<'a>,
        lhs: NodeVal<'a, L>,
        rhs: NodeVal<'a, L>,
    ) -> Value<'a, L>;
}

#[derive(Debug, Clone)]
pub enum Value<'a, L: AssemblyLanguage<'a>> {
    Constant(Constant<'a>),
    Label(LabelUse<'a>),
    Indexed(L::Indexed),
    Register(L::RegType),
    Custom(L::CustomValue)
}

impl<'a, L: AssemblyLanguage<'a>> core::cmp::PartialEq for Value<'a, L>
where
    L: AssemblyLanguage<'a>,
    L::RegType: core::cmp::PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Constant(f0_self), Value::Constant(f0_other)) => f0_self.eq(f0_other),
            (Value::Label(f0_self), Value::Label(f0_other)) => f0_self.eq(f0_other),
            (Value::Indexed(f0_self), Value::Indexed(f0_other)) => f0_self.eq(f0_other),
            (Value::Register(f0_self), Value::Register(f0_other)) => f0_self.eq(f0_other),
            _unused => false,
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> Copy for Value<'a, L>
where
    L::RegType: Copy,
    L::Indexed: Copy,
{
}

impl<'a, L: AssemblyLanguage<'a>> Display for Value<'a, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Value::Constant(c) => write!(f, "{c}"),
            Value::Label(l) => write!(f, "{l}"),
            Value::Indexed(index) => write!(f, "{index}"),
            Value::Register(reg) => write!(f, "{reg}"),
            Value::Custom(custom) => write!(f, "{custom}"),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> Value<'a, L> {
    pub fn get_type(&self) -> ValueType<'a, L> {
        match self {
            Value::Constant(c) => match c {
                Constant::I8(_) => ValueType::I8,
                Constant::I16(_) => ValueType::I16,
                Constant::I32(_) => ValueType::I32,
                Constant::I64(_) => ValueType::I64,
                Constant::U8(_) => ValueType::U8,
                Constant::U16(_) => ValueType::U16,
                Constant::U32(_) => ValueType::U32,
                Constant::U64(_) => ValueType::U64,
                Constant::F32(_) => ValueType::F32,
                Constant::F64(_) => ValueType::F64,
                Constant::String(_) => ValueType::String,
                Constant::Char(_) => ValueType::Char,
                Constant::Bool(_) => ValueType::Bool,
            },
            Value::Label(_) => ValueType::Label,
            Value::Indexed(_) => ValueType::Indexed,
            Value::Register(_) => ValueType::Register,
            Value::Custom(c) => ValueType::Custom(c.get_type())
        }
    }

    pub fn get_size(&self) -> Option<u32> {
        match *self {
            Value::Constant(c) => match c {
                Constant::I8(_) => Some(1),
                Constant::I16(_) => Some(2),
                Constant::I32(_) => Some(4),
                Constant::I64(_) => Some(8),
                Constant::U8(_) => Some(1),
                Constant::U16(_) => Some(2),
                Constant::U32(_) => Some(4),
                Constant::U64(_) => Some(8),
                Constant::F32(_) => Some(4),
                Constant::F64(_) => Some(8),
                Constant::String(str) => Some(str.len() as u32),
                Constant::Char(_) => Some(4),
                Constant::Bool(_) => Some(1),
            },
            Value::Label(_) => Some(4),
            Value::Indexed(_) => None,
            Value::Register(_) => None,
            Value::Custom(c) => c.get_size()
        }
    }

    pub fn get_align(&self) -> Option<u32> {
        match *self {
            Value::Constant(c) => match c {
                Constant::I8(_) => Some(1),
                Constant::I16(_) => Some(2),
                Constant::I32(_) => Some(4),
                Constant::I64(_) => Some(8),
                Constant::U8(_) => Some(1),
                Constant::U16(_) => Some(2),
                Constant::U32(_) => Some(4),
                Constant::U64(_) => Some(8),
                Constant::F32(_) => Some(4),
                Constant::F64(_) => Some(8),
                Constant::String(_) => Some(1),
                Constant::Char(_) => Some(4),
                Constant::Bool(_) => Some(1),
            },
            Value::Label(_) => Some(4),
            Value::Indexed(_) => None,
            Value::Register(_) => None,
            Value::Custom(c) => c.get_align()
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Constant<'a> {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),

    F32(f32),
    F64(f64),

    String(&'a str),
    Char(char),
    Bool(bool),
}

impl<'a> Display for Constant<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Constant::I8(c) => write!(f, "{c}"),
            Constant::I16(c) => write!(f, "{c}"),
            Constant::I32(c) => write!(f, "{c}"),
            Constant::I64(c) => write!(f, "{c}"),
            Constant::U8(c) => write!(f, "{c}"),
            Constant::U16(c) => write!(f, "{c}"),
            Constant::U32(c) => write!(f, "{c}"),
            Constant::U64(c) => write!(f, "{c}"),
            Constant::F32(c) => write!(f, "{c}"),
            Constant::F64(c) => write!(f, "{c}"),
            Constant::String(c) => write!(f, "{c}"),
            Constant::Char(c) => write!(f, "{c}"),
            Constant::Bool(c) => write!(f, "{c}"),
        }
    }
}

pub enum ConvertResult<T> {
    Success(T),
    Lossy(T),
    Failure,
}

macro_rules! conversion {
    ($func:ident, $into:ty, $($try:ident)*, $($cast:ident)*) => {
        #[allow(warnings)]
        pub fn $func(&self) -> ConvertResult<$into> {
            match *self {
                $(
                    Constant::$try(v) => if let Ok(ok) = v.try_into() { ConvertResult::Success(ok)} else {ConvertResult::Lossy(v as $into)},
                )*
                $(
                    Constant::$cast(v) => ConvertResult::Success(v as $into),
                )*
                _ => ConvertResult::Failure,
            }
        }
    };
}

impl<'a> Constant<'a> {
    conversion!(to_u8, u8, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);
    conversion!(to_u16, u16, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);
    conversion!(to_u32, u32, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);
    conversion!(to_u64, u64, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);

    conversion!(to_i8, i8, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);
    conversion!(to_i16, i16, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);
    conversion!(to_i32, i32, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);
    conversion!(to_i64, i64, I8 I16 I32 I64 U8 U16 U32 U64, Char Bool);

    conversion!(to_f32, f32, ,I8 I16 I32 I64 U8 U16 U32 U64 F32 F64);
    conversion!(to_f64, f64, ,I8 I16 I32 I64 U8 U16 U32 U64 F32 F64);
}

impl<'a, L: AssemblyLanguage<'a>> ValueType<'a, L> {
    pub fn is_numeric(&self) -> bool {
        self.numeric_suffix().is_some()
    }
    pub fn is_integer(&self) -> bool {
        match self {
            ValueType::Any => false,
            ValueType::Register => false,
            ValueType::Indexed => false,
            ValueType::String => false,
            ValueType::Label => false,
            ValueType::I8 => true,
            ValueType::I16 => true,
            ValueType::I32 => true,
            ValueType::I64 => true,
            ValueType::U8 => true,
            ValueType::U16 => true,
            ValueType::U32 => true,
            ValueType::U64 => true,
            ValueType::F32 => false,
            ValueType::F64 => false,
            ValueType::Bool => false,
            ValueType::Char => false,
            ValueType::Custom(_) => false,
        }
    }
    pub fn numeric_suffix(&self) -> Option<&'static str> {
        match self {
            ValueType::Any => None,
            ValueType::Register => None,
            ValueType::Indexed => None,
            ValueType::String => None,
            ValueType::Label => None,
            ValueType::I8 => Some("i8"),
            ValueType::I16 => Some("i16"),
            ValueType::I32 => Some("i32"),
            ValueType::I64 => Some("i64"),
            ValueType::U8 => Some("u8"),
            ValueType::U16 => Some("u16"),
            ValueType::U32 => Some("u32"),
            ValueType::U64 => Some("u64"),
            ValueType::F32 => Some("f32"),
            ValueType::F64 => Some("f64"),
            ValueType::Bool => None,
            ValueType::Char => None,
            ValueType::Custom(_) => None,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ArgumentsTypeHint<'a, L: AssemblyLanguage<'a>> {
    Mono(ValueType<'a, L>),
    Individual(&'a [ValueType<'a, L>]),
    None,
}

impl<'a, L: AssemblyLanguage<'a>> Index<usize> for ArgumentsTypeHint<'a, L> {
    type Output = ValueType<'a, L>;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            ArgumentsTypeHint::Mono(ty) => ty,
            ArgumentsTypeHint::Individual(ty) => ty.get(index).unwrap_or(&ValueType::Any),
            ArgumentsTypeHint::None => &ValueType::Any,
        }
    }
}
