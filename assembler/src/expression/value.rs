use crate::assembler::instructions::Register;
use std::fmt::{Display, Formatter};
use std::ops::Index;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ValueType {
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
}

impl Display for ValueType {
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
        }
    }
}

impl ValueType {
    pub fn default_value<'a>(&self) -> Value<'a> {
        match self {
            ValueType::Any => Value::Constant(Constant::I32(0)),
            ValueType::String => Value::Constant(Constant::String("")),
            ValueType::Indexed => Value::LabelRegisterOffset(
                Register(0),
                LabelUse {
                    ident: "",
                    offset: 0,
                    meta: LabelMeta::Unset,
                },
            ),
            ValueType::Register => Value::Register(Register(0)),
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value<'a> {
    Constant(Constant<'a>),
    Label(LabelUse<'a>),
    LabelRegisterOffset(Register, LabelUse<'a>),
    RegisterOffset(Register, i32),
    Register(Register),
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Label(l) => write!(f, "{}", l),
            Value::LabelRegisterOffset(reg, l) => {
                if l.meta.is_unset() && l.offset != 0 {
                    write!(f, "(")?;
                }
                write!(f, "{}", l)?;
                if l.meta.is_unset() && l.offset != 0 {
                    write!(f, ")")?;
                }
                if reg.0 != 0 {
                    write!(f, "[{reg}]")?;
                }
                Ok(())
            }
            Value::RegisterOffset(reg, offset) => {
                if reg.0 == 0 {
                    write!(f, "{offset}")?;
                } else {
                    write!(f, "{reg}")?;
                    if offset != 0 {
                        write!(f, "[{offset}]")?;
                    }
                }
                Ok(())
            }
            Value::Register(reg) => write!(f, "{}", reg),
        }
    }
}

impl<'a> Value<'a> {
    pub fn get_type(&self) -> ValueType {
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
            Value::LabelRegisterOffset(_, _) => ValueType::Indexed,
            Value::RegisterOffset(_, _) => ValueType::Indexed,
            Value::Register(_) => ValueType::Register,
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
            Value::LabelRegisterOffset(_, _) => None,
            Value::RegisterOffset(_, _) => None,
            Value::Register(_) => None,
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
            Value::LabelRegisterOffset(_, _) => None,
            Value::RegisterOffset(_, _) => None,
            Value::Register(_) => None,
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
            Constant::I8(c) => write!(f, "{}", c),
            Constant::I16(c) => write!(f, "{}", c),
            Constant::I32(c) => write!(f, "{}", c),
            Constant::I64(c) => write!(f, "{}", c),
            Constant::U8(c) => write!(f, "{}", c),
            Constant::U16(c) => write!(f, "{}", c),
            Constant::U32(c) => write!(f, "{}", c),
            Constant::U64(c) => write!(f, "{}", c),
            Constant::F32(c) => write!(f, "{}", c),
            Constant::F64(c) => write!(f, "{}", c),
            Constant::String(c) => write!(f, "{}", c),
            Constant::Char(c) => write!(f, "{}", c),
            Constant::Bool(c) => write!(f, "{}", c),
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

impl ValueType {
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
        }
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
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ArgumentsTypeHint<'a> {
    Mono(ValueType),
    Individual(&'a [ValueType]),
    None,
}

impl<'a> Index<usize> for ArgumentsTypeHint<'a> {
    type Output = ValueType;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            ArgumentsTypeHint::Mono(ty) => ty,
            ArgumentsTypeHint::Individual(ty) => ty.get(index).unwrap_or(&ValueType::Any),
            ArgumentsTypeHint::None => &ValueType::Any,
        }
    }
}
