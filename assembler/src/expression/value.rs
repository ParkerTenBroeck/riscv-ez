use std::convert::Infallible;
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::ops::Index;

use num_traits::FromPrimitive;

use crate::assembler::lang::AssemblyLanguage;
use crate::config::ImplicitCastConfig;
use crate::context::{Context, NodeId};
use crate::expression::AsmStr;

pub trait AssemblyLabel<'a>:
    Sized + Default + std::fmt::Display + std::fmt::Debug + Copy + Clone + Eq + PartialEq
{
    type Lang: AssemblyLanguage<'a>;

    fn get_size(&self) -> Option<<Self::Lang as AssemblyLanguage<'a>>::Uptr> {
        <Self::Lang as AssemblyLanguage<'a>>::Uptr::from_usize(std::mem::size_of::<
            <Self::Lang as AssemblyLanguage<'a>>::Uptr,
        >())
    }
    fn get_align(&self) -> Option<<Self::Lang as AssemblyLanguage<'a>>::Uptr> {
        <Self::Lang as AssemblyLanguage<'a>>::Uptr::from_usize(std::mem::align_of::<
            <Self::Lang as AssemblyLanguage<'a>>::Uptr,
        >())
    }
    type Offset: ImplicitCastFrom<'a, Constant<'a, Self::Lang>> + Default;
}

pub trait AssemblyRegister<'a>:
    std::fmt::Debug + Clone + Copy + PartialEq + std::fmt::Display + Default + Sized
{
    type Lang: AssemblyLanguage<'a>;
}
pub trait Indexed<'a>:
    std::fmt::Debug + Clone + Copy + PartialEq + std::fmt::Display + Default + Sized
{
    type Lang: AssemblyLanguage<'a>;
}

pub trait CustomValue<'a>:
    std::fmt::Debug + Clone + Copy + PartialEq + Eq + std::fmt::Display + Sized
{
    type Lang: AssemblyLanguage<'a>;
    type CustomValueType: CustomValueType<'a, CustomValue = Self>;
    fn get_align(&self) -> Option<<Self::Lang as AssemblyLanguage<'a>>::Uptr>;
    fn get_size(&self) -> Option<<Self::Lang as AssemblyLanguage<'a>>::Uptr>;
    fn get_type(&self) -> Self::CustomValueType;
}
pub trait CustomValueType<'a>:
    std::fmt::Debug + Clone + Copy + PartialEq + Eq + std::fmt::Display + Sized
{
    type Lang: AssemblyLanguage<'a>;
    type CustomValue: CustomValue<'a, CustomValueType = Self>;
    fn default_value(&self) -> Self::CustomValue;
}

//----------------------------------------------------------------------------

#[derive(Eq)]
pub enum ValueType<'a, L: AssemblyLanguage<'a>> {
    Any,

    Str,

    Indexed,
    Register,

    Label,

    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    Iptr,

    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    Uptr,

    F32,
    F64,

    Bool,
    Char,

    Type,

    Custom(<L::CustomValue as CustomValue<'a>>::CustomValueType),
}

impl<'a, L: AssemblyLanguage<'a>> ValueType<'a, L> {
    pub fn get_align(&self) -> Option<L::Uptr> {
        match self {
            Self::I8 => <L::Uptr as FromPrimitive>::from_usize(1),
            Self::I16 => <L::Uptr as FromPrimitive>::from_usize(2),
            Self::I32 => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::I64 => <L::Uptr as FromPrimitive>::from_usize(8),
            Self::I128 => <L::Uptr as FromPrimitive>::from_usize(16),
            Self::Isize => <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Isize>()),
            Self::Iptr => <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Iptr>()),
            Self::U8 => <L::Uptr as FromPrimitive>::from_usize(1),
            Self::U16 => <L::Uptr as FromPrimitive>::from_usize(2),
            Self::U32 => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::U64 => <L::Uptr as FromPrimitive>::from_usize(8),
            Self::U128 => <L::Uptr as FromPrimitive>::from_usize(16),
            Self::Usize => <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Usize>()),
            Self::Uptr => <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Uptr>()),
            Self::F32 => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::F64 => <L::Uptr as FromPrimitive>::from_usize(8),
            Self::Str => <L::Uptr as FromPrimitive>::from_usize(1),
            Self::Char => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::Bool => <L::Uptr as FromPrimitive>::from_usize(1),
            _ => Some(num_traits::one()),
        }
    }

    pub fn get_size(&self) -> Option<L::Uptr> {
        match self {
            Self::I8 => <L::Uptr as FromPrimitive>::from_usize(1),
            Self::I16 => <L::Uptr as FromPrimitive>::from_usize(2),
            Self::I32 => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::I64 => <L::Uptr as FromPrimitive>::from_usize(8),
            Self::I128 => <L::Uptr as FromPrimitive>::from_usize(16),
            Self::Isize => <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Isize>()),
            Self::Iptr => <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Iptr>()),
            Self::U8 => <L::Uptr as FromPrimitive>::from_usize(1),
            Self::U16 => <L::Uptr as FromPrimitive>::from_usize(2),
            Self::U32 => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::U64 => <L::Uptr as FromPrimitive>::from_usize(8),
            Self::U128 => <L::Uptr as FromPrimitive>::from_usize(16),
            Self::Usize => <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Usize>()),
            Self::Uptr => <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Uptr>()),
            Self::F32 => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::F64 => <L::Uptr as FromPrimitive>::from_usize(8),
            Self::Char => <L::Uptr as FromPrimitive>::from_usize(4),
            Self::Bool => <L::Uptr as FromPrimitive>::from_usize(1),

            _ => Some(num_traits::zero()),
        }
    }
}

impl<'a, L> core::fmt::Debug for ValueType<'a, L>
where
    L: AssemblyLanguage<'a>,
{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            ValueType::Any => f.write_str("Any"),
            ValueType::Str => f.write_str("Str"),
            ValueType::Indexed => f.write_str("Indexed"),
            ValueType::Register => f.write_str("Register"),
            ValueType::Label => f.write_str("Label"),
            ValueType::I8 => f.write_str("I8"),
            ValueType::I16 => f.write_str("I16"),
            ValueType::I32 => f.write_str("I32"),
            ValueType::I64 => f.write_str("I64"),
            ValueType::I128 => f.write_str("I128"),
            ValueType::Isize => f.write_str("Isize"),
            ValueType::Iptr => f.write_str("Iptr"),
            ValueType::U8 => f.write_str("U8"),
            ValueType::U16 => f.write_str("U16"),
            ValueType::U32 => f.write_str("U32"),
            ValueType::U64 => f.write_str("U64"),
            ValueType::U128 => f.write_str("U128"),
            ValueType::Usize => f.write_str("Usize"),
            ValueType::Uptr => f.write_str("Uptr"),
            ValueType::F32 => f.write_str("F32"),
            ValueType::F64 => f.write_str("F64"),
            ValueType::Bool => f.write_str("Bool"),
            ValueType::Char => f.write_str("Char"),
            ValueType::Type => f.write_str("Type"),
            ValueType::Custom(f0) => f.debug_tuple("Custom").field(&f0).finish(),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> core::cmp::PartialEq for ValueType<'a, L> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ValueType::Any, ValueType::Any) => true,
            (ValueType::Str, ValueType::Str) => true,
            (ValueType::Indexed, ValueType::Indexed) => true,
            (ValueType::Register, ValueType::Register) => true,
            (ValueType::Label, ValueType::Label) => true,
            (ValueType::I8, ValueType::I8) => true,
            (ValueType::I16, ValueType::I16) => true,
            (ValueType::I32, ValueType::I32) => true,
            (ValueType::I64, ValueType::I64) => true,
            (ValueType::I128, ValueType::I128) => true,
            (ValueType::Isize, ValueType::Isize) => true,
            (ValueType::Iptr, ValueType::Iptr) => true,
            (ValueType::U8, ValueType::U8) => true,
            (ValueType::U16, ValueType::U16) => true,
            (ValueType::U32, ValueType::U32) => true,
            (ValueType::U64, ValueType::U64) => true,
            (ValueType::U128, ValueType::U128) => true,
            (ValueType::Usize, ValueType::Usize) => true,
            (ValueType::Uptr, ValueType::Uptr) => true,
            (ValueType::F32, ValueType::F32) => true,
            (ValueType::F64, ValueType::F64) => true,
            (ValueType::Bool, ValueType::Bool) => true,
            (ValueType::Char, ValueType::Char) => true,
            (ValueType::Custom(f0_self), ValueType::Custom(f0_other)) => f0_self.eq(f0_other),
            _unused => false,
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> Clone for ValueType<'a, L> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, L: AssemblyLanguage<'a>> Copy for ValueType<'a, L> {}

impl<'a, L: AssemblyLanguage<'a>> std::fmt::Display for ValueType<'a, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Any => write!(f, "any"),
            ValueType::Str => write!(f, "str"),
            ValueType::Indexed => write!(f, "indexed"),
            ValueType::Register => write!(f, "register"),
            ValueType::Label => write!(f, "label"),
            ValueType::I8 => write!(f, "i8"),
            ValueType::I16 => write!(f, "i16"),
            ValueType::I32 => write!(f, "i32"),
            ValueType::I64 => write!(f, "i64"),
            ValueType::I128 => write!(f, "i128"),
            ValueType::Isize => write!(f, "isize"),
            ValueType::Iptr => write!(f, "iptr"),
            ValueType::U8 => write!(f, "u8"),
            ValueType::U16 => write!(f, "u16"),
            ValueType::U32 => write!(f, "u32"),
            ValueType::U64 => write!(f, "u64"),
            ValueType::Usize => write!(f, "usize"),
            ValueType::Uptr => write!(f, "uptr"),
            ValueType::U128 => write!(f, "u128"),
            ValueType::F32 => write!(f, "f32"),
            ValueType::F64 => write!(f, "f64"),
            ValueType::Bool => write!(f, "bool"),
            ValueType::Char => write!(f, "char"),
            ValueType::Type => write!(f, "type"),
            ValueType::Custom(custom) => write!(f, "{custom}"),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> ValueType<'a, L> {
    pub fn default_value(&self) -> Value<'a, L> {
        match self {
            ValueType::Any => Value::Constant(Constant::I32(0)),
            ValueType::Type => Value::Type(ValueType::Any),
            ValueType::Str => Value::Constant(Constant::Str(Default::default())),
            ValueType::Indexed => Value::Indexed(L::Indexed::default()),
            ValueType::Register => Value::Register(L::Reg::default()),
            ValueType::Label => Value::Label(Default::default()),
            ValueType::I8 => Value::Constant(Constant::I8(0)),
            ValueType::I16 => Value::Constant(Constant::I16(0)),
            ValueType::I32 => Value::Constant(Constant::I32(0)),
            ValueType::I64 => Value::Constant(Constant::I64(0)),
            ValueType::I128 => Value::Constant(Constant::I128(0)),
            ValueType::Iptr => Value::Constant(Constant::Iptr(Default::default())),
            ValueType::Isize => Value::Constant(Constant::Isize(Default::default())),
            ValueType::U8 => Value::Constant(Constant::U8(0)),
            ValueType::U16 => Value::Constant(Constant::U16(0)),
            ValueType::U32 => Value::Constant(Constant::U32(0)),
            ValueType::U64 => Value::Constant(Constant::U64(0)),
            ValueType::U128 => Value::Constant(Constant::U128(0)),
            ValueType::Uptr => Value::Constant(Constant::Uptr(Default::default())),
            ValueType::Usize => Value::Constant(Constant::Usize(Default::default())),
            ValueType::F32 => Value::Constant(Constant::F32(0.0)),
            ValueType::F64 => Value::Constant(Constant::F64(0.0)),
            ValueType::Bool => Value::Constant(Constant::Bool(false)),
            ValueType::Char => Value::Constant(Constant::Char('\0')),
            ValueType::Custom(c) => Value::Custom(c.default_value()),
        }
    }
}

//----------------------------------------------------------------------------

pub enum EmptyCustomValue<L> {
    __(Infallible, PhantomData<L>),
}

impl<T> Copy for EmptyCustomValue<T> {}
impl<T> Clone for EmptyCustomValue<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> std::fmt::Display for EmptyCustomValue<T> {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
impl<T> std::fmt::Debug for EmptyCustomValue<T> {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        Ok(())
    }
}
impl<T> Eq for EmptyCustomValue<T> {}
impl<T> PartialEq for EmptyCustomValue<T> {
    fn eq(&self, _: &Self) -> bool {
        false
    }
}

impl<'a, L: AssemblyLanguage<'a>> CustomValueType<'a> for EmptyCustomValue<L> {
    type Lang = L;
    type CustomValue = EmptyCustomValue<L>;
    fn default_value(&self) -> Self::CustomValue {
        unreachable!()
    }
}

impl<'a, L: AssemblyLanguage<'a>> CustomValue<'a> for EmptyCustomValue<L> {
    type Lang = L;
    type CustomValueType = EmptyCustomValue<L>;
    fn get_align(&self) -> Option<L::Uptr> {
        unreachable!()
    }

    fn get_size(&self) -> Option<L::Uptr> {
        unreachable!()
    }

    fn get_type(&self) -> Self::CustomValueType {
        unreachable!()
    }
}

//----------------------------------------------------------------------------

pub enum Value<'a, L: AssemblyLanguage<'a>> {
    Constant(Constant<'a, L>),
    Label(L::Label),
    Indexed(L::Indexed),
    Register(L::Reg),
    Custom(L::CustomValue),
    Type(ValueType<'a, L>),
}

impl<'a, L> core::fmt::Debug for Value<'a, L>
where
    L: AssemblyLanguage<'a>,
    L::Label: core::fmt::Debug,
    L::Indexed: core::fmt::Debug,
    L::Reg: core::fmt::Debug,
    L::CustomValue: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Value::Constant(f0) => f.debug_tuple("Constant").field(&f0).finish(),
            Value::Label(f0) => f.debug_tuple("Label").field(&f0).finish(),
            Value::Indexed(f0) => f.debug_tuple("Indexed").field(&f0).finish(),
            Value::Register(f0) => f.debug_tuple("Register").field(&f0).finish(),
            Value::Custom(f0) => f.debug_tuple("Custom").field(&f0).finish(),
            Value::Type(f0) => f.debug_tuple("Type").field(&f0).finish(),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> core::cmp::PartialEq for Value<'a, L> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Constant(f0_self), Value::Constant(f0_other)) => f0_self.eq(f0_other),
            (Value::Label(f0_self), Value::Label(f0_other)) => f0_self.eq(f0_other),
            (Value::Indexed(f0_self), Value::Indexed(f0_other)) => f0_self.eq(f0_other),
            (Value::Register(f0_self), Value::Register(f0_other)) => f0_self.eq(f0_other),
            (Value::Type(f0_self), Value::Type(f0_other)) => f0_self.eq(f0_other),
            _unused => false,
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> Clone for Value<'a, L> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, L: AssemblyLanguage<'a>> Copy for Value<'a, L> {}

impl<'a, L: AssemblyLanguage<'a>> std::fmt::Display for Value<'a, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Value::Constant(c) => write!(f, "{c}"),
            Value::Label(l) => write!(f, "{l}"),
            Value::Indexed(index) => write!(f, "{index}"),
            Value::Register(reg) => write!(f, "{reg}"),
            Value::Custom(custom) => write!(f, "{custom}"),
            Value::Type(ty) => write!(f, "{ty}"),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> Value<'a, L> {
    pub fn get_type(&self) -> ValueType<'a, L> {
        match self {
            Value::Constant(c) => c.get_type(),
            Value::Label(_) => ValueType::Label,
            Value::Indexed(_) => ValueType::Indexed,
            Value::Register(_) => ValueType::Register,
            Value::Custom(c) => ValueType::Custom(c.get_type()),
            Value::Type(_) => ValueType::Type,
        }
    }

    pub fn get_size(&self) -> Option<L::Uptr> {
        match *self {
            Value::Constant(c) => c.get_size(),
            Value::Label(l) => l.get_size(),
            Value::Indexed(_) => None,
            Value::Register(_) => None,
            Value::Custom(c) => c.get_size(),
            Value::Type(ty) => ty.get_size(),
        }
    }

    pub fn get_align(&self) -> Option<L::Uptr> {
        match *self {
            Value::Constant(c) => c.get_align(),
            Value::Label(l) => l.get_align(),
            Value::Indexed(_) => None,
            Value::Register(_) => None,
            Value::Custom(c) => c.get_align(),
            Value::Type(ty) => ty.get_align(),
        }
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Value::Constant(Constant::Bool(true)))
    }
}

//----------------------------------------------------------------------------

pub enum Constant<'a, L: AssemblyLanguage<'a>> {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),

    Isize(L::Isize),
    Iptr(L::Iptr),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),

    Usize(L::Usize),
    Uptr(L::Uptr),

    F32(f32),
    F64(f64),

    Str(AsmStr<'a>),
    Char(char),
    Bool(bool),
}
impl<'a, L: AssemblyLanguage<'a>> Copy for Constant<'a, L> {}
impl<'a, L: AssemblyLanguage<'a>> Clone for Constant<'a, L> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<'a, L: AssemblyLanguage<'a>> PartialEq for Constant<'a, L> {
    fn eq(&self, other: &Constant<'a, L>) -> bool {
        match (self, other) {
            (Constant::I8(lhs), Constant::I8(rhs)) => lhs == rhs,
            (Constant::I16(lhs), Constant::I16(rhs)) => lhs == rhs,
            (Constant::I32(lhs), Constant::I32(rhs)) => lhs == rhs,
            (Constant::I64(lhs), Constant::I64(rhs)) => lhs == rhs,
            (Constant::I128(lhs), Constant::I128(rhs)) => lhs == rhs,
            (Constant::Isize(lhs), Constant::Isize(rhs)) => lhs == rhs,
            (Constant::Iptr(lhs), Constant::Iptr(rhs)) => lhs == rhs,
            (Constant::U8(lhs), Constant::U8(rhs)) => lhs == rhs,
            (Constant::U16(lhs), Constant::U16(rhs)) => lhs == rhs,
            (Constant::U32(lhs), Constant::U32(rhs)) => lhs == rhs,
            (Constant::U64(lhs), Constant::U64(rhs)) => lhs == rhs,
            (Constant::U128(lhs), Constant::U128(rhs)) => lhs == rhs,
            (Constant::Usize(lhs), Constant::Usize(rhs)) => lhs == rhs,
            (Constant::Uptr(lhs), Constant::Uptr(rhs)) => lhs == rhs,
            (Constant::F32(lhs), Constant::F32(rhs)) => lhs == rhs,
            (Constant::F64(lhs), Constant::F64(rhs)) => lhs == rhs,
            (Constant::Str(lhs), Constant::Str(rhs)) => lhs == rhs,
            (Constant::Char(lhs), Constant::Char(rhs)) => lhs == rhs,
            (Constant::Bool(lhs), Constant::Bool(rhs)) => lhs == rhs,
            (_, _) => false,
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> std::fmt::Debug for Constant<'a, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::I8(c) => c.fmt(f),
            Constant::I16(c) => c.fmt(f),
            Constant::I32(c) => c.fmt(f),
            Constant::I64(c) => c.fmt(f),
            Constant::I128(c) => c.fmt(f),
            Constant::Isize(c) => c.fmt(f),
            Constant::Iptr(c) => c.fmt(f),
            Constant::U8(c) => c.fmt(f),
            Constant::U16(c) => c.fmt(f),
            Constant::U32(c) => c.fmt(f),
            Constant::U64(c) => c.fmt(f),
            Constant::U128(c) => c.fmt(f),
            Constant::Usize(c) => c.fmt(f),
            Constant::Uptr(c) => c.fmt(f),
            Constant::F32(c) => c.fmt(f),
            Constant::F64(c) => c.fmt(f),
            Constant::Str(c) => c.fmt(f),
            Constant::Char(c) => c.fmt(f),
            Constant::Bool(c) => c.fmt(f),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> Constant<'a, L> {
    pub fn is_float(&self) -> bool {
        matches!(self, Self::F32(_) | Self::F64(_))
    }

    pub fn get_type(&self) -> ValueType<'a, L> {
        match self {
            Constant::I8(_) => ValueType::I8,
            Constant::I16(_) => ValueType::I16,
            Constant::I32(_) => ValueType::I32,
            Constant::I64(_) => ValueType::I64,
            Constant::I128(_) => ValueType::I128,
            Constant::Isize(_) => ValueType::Isize,
            Constant::Iptr(_) => ValueType::Iptr,
            Constant::U8(_) => ValueType::U8,
            Constant::U16(_) => ValueType::U16,
            Constant::U32(_) => ValueType::U32,
            Constant::U64(_) => ValueType::U64,
            Constant::U128(_) => ValueType::U128,
            Constant::Usize(_) => ValueType::Usize,
            Constant::Uptr(_) => ValueType::Uptr,
            Constant::F32(_) => ValueType::F32,
            Constant::F64(_) => ValueType::F64,
            Constant::Str(_) => ValueType::Str,
            Constant::Char(_) => ValueType::Char,
            Constant::Bool(_) => ValueType::Bool,
        }
    }

    pub fn is_numeric(&self) -> bool {
        self.is_float() || self.is_integer()
    }

    pub fn is_integer(&self) -> bool {
        self.is_signed_integer() || self.is_unsigned_integer()
    }

    pub fn is_unsigned_integer(&self) -> bool {
        matches!(
            self,
            Self::U8(_) | Self::U16(_) | Self::U32(_) | Self::U64(_)
        )
    }

    pub fn is_signed_integer(&self) -> bool {
        matches!(
            self,
            Self::I8(_) | Self::I16(_) | Self::I32(_) | Self::I64(_)
        )
    }

    pub fn get_align(&self) -> Option<L::Uptr> {
        match self {
            Constant::I8(_) => <L::Uptr as FromPrimitive>::from_usize(1),
            Constant::I16(_) => <L::Uptr as FromPrimitive>::from_usize(2),
            Constant::I32(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::I64(_) => <L::Uptr as FromPrimitive>::from_usize(8),
            Constant::I128(_) => <L::Uptr as FromPrimitive>::from_usize(16),
            Constant::Isize(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Isize>())
            }
            Constant::Iptr(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Iptr>())
            }
            Constant::U8(_) => <L::Uptr as FromPrimitive>::from_usize(1),
            Constant::U16(_) => <L::Uptr as FromPrimitive>::from_usize(2),
            Constant::U32(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::U64(_) => <L::Uptr as FromPrimitive>::from_usize(8),
            Constant::U128(_) => <L::Uptr as FromPrimitive>::from_usize(16),
            Constant::Usize(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Usize>())
            }
            Constant::Uptr(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::align_of::<L::Uptr>())
            }
            Constant::F32(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::F64(_) => <L::Uptr as FromPrimitive>::from_usize(8),
            Constant::Str(_) => <L::Uptr as FromPrimitive>::from_usize(1),
            Constant::Char(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::Bool(_) => <L::Uptr as FromPrimitive>::from_usize(1),
        }
    }

    pub fn get_size(&self) -> Option<L::Uptr> {
        match self {
            Constant::I8(_) => <L::Uptr as FromPrimitive>::from_usize(1),
            Constant::I16(_) => <L::Uptr as FromPrimitive>::from_usize(2),
            Constant::I32(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::I64(_) => <L::Uptr as FromPrimitive>::from_usize(8),
            Constant::I128(_) => <L::Uptr as FromPrimitive>::from_usize(16),
            Constant::Isize(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Isize>())
            }
            Constant::Iptr(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Iptr>())
            }
            Constant::U8(_) => <L::Uptr as FromPrimitive>::from_usize(1),
            Constant::U16(_) => <L::Uptr as FromPrimitive>::from_usize(2),
            Constant::U32(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::U64(_) => <L::Uptr as FromPrimitive>::from_usize(8),
            Constant::U128(_) => <L::Uptr as FromPrimitive>::from_usize(16),
            Constant::Usize(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Usize>())
            }
            Constant::Uptr(_) => {
                <L::Uptr as FromPrimitive>::from_usize(std::mem::size_of::<L::Uptr>())
            }
            Constant::F32(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::F64(_) => <L::Uptr as FromPrimitive>::from_usize(8),
            Constant::Str(str) => <L::Uptr as FromPrimitive>::from_usize(str.len()),
            Constant::Char(_) => <L::Uptr as FromPrimitive>::from_usize(4),
            Constant::Bool(_) => <L::Uptr as FromPrimitive>::from_usize(1),
        }
    }
}

impl<'a, L: AssemblyLanguage<'a>> std::fmt::Display for Constant<'a, L> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::I8(c) => c.fmt(f),
            Constant::I16(c) => c.fmt(f),
            Constant::I32(c) => c.fmt(f),
            Constant::I64(c) => c.fmt(f),
            Constant::I128(c) => c.fmt(f),
            Constant::Isize(c) => c.fmt(f),
            Constant::Iptr(c) => c.fmt(f),
            Constant::U8(c) => c.fmt(f),
            Constant::U16(c) => c.fmt(f),
            Constant::U32(c) => c.fmt(f),
            Constant::U64(c) => c.fmt(f),
            Constant::U128(c) => c.fmt(f),
            Constant::Usize(c) => c.fmt(f),
            Constant::Uptr(c) => c.fmt(f),
            Constant::F32(c) => c.fmt(f),
            Constant::F64(c) => c.fmt(f),
            Constant::Str(c) => c.fmt(f),
            Constant::Char(c) => c.fmt(f),
            Constant::Bool(c) => c.fmt(f),
        }
    }
}

//----------------------------------------------------------------------------

pub trait ImplicitCastTo<'a, To>
where
    Self: Sized,
{
    fn cast(self, node: NodeId<'a>, ctx: &mut Context<'a>) -> Option<To> {
        Self::cast_with(self, node, ctx, ctx.config().implicit_cast_defaults)
    }
    fn cast_with(
        self,
        node: NodeId<'a>,
        ctx: &mut Context<'a>,
        cfg: ImplicitCastConfig,
    ) -> Option<To>;
}

pub trait ImplicitCastFrom<'a, From>
where
    Self: Sized,
{
    fn cast_with(
        from: From,
        node: NodeId<'a>,
        ctx: &mut Context<'a>,
        cfg: ImplicitCastConfig,
    ) -> Option<Self>;
}

impl<'a, To: ImplicitCastFrom<'a, From>, From> ImplicitCastTo<'a, To> for From {
    fn cast_with(
        self,
        node: NodeId<'a>,
        ctx: &mut Context<'a>,
        cfg: ImplicitCastConfig,
    ) -> Option<To> {
        To::cast_with(self, node, ctx, cfg)
    }
}

macro_rules! implicit_cast_impl {
    ($into:ty, identity: $identity:ident, $(identity_inv_sign: $identity_sign_change:ident,)? $(fnarrow: $fnarrow:ident,)? $(fwide: $fwide:ident,)? narrowing: [$($narrowing:ident),*], widening: [$($widening:ident),*], f2i: [$($f2i:ident),*], i2f: [$($i2f:ident),*] $(,)?) => {
        impl<'a, L: AssemblyLanguage<'a>> ImplicitCastFrom<'a, Constant<'a,  L>> for $into {
            fn cast_with(
                from: Constant<'a, L>,
                node: NodeId<'a>,
                ctx: &mut Context<'a>,
                cfg: ImplicitCastConfig,
            ) -> Option<$into> {
                use $crate::config::LogOn as LO;
                use $crate::logs::LogEntry;

                match from {
                    Constant::$identity(i) => Some(i),
                    $(Constant::$identity_sign_change(i) => match cfg.sign {
                        LO::Error => implicit_cast_impl!(!error, node, ctx, $identity_sign_change, $into),
                        LO::Warning => implicit_cast_impl!(!warning, node, ctx, i, cfg.lossy, $identity_sign_change, $into),
                        LO::None => implicit_cast_impl!(!try_into, node, ctx, i, cfg.lossy, $identity_sign_change, $into),
                    },)?
                    $(
                        Constant::$fnarrow(i) => match cfg.narrowing {
                            LO::Error => implicit_cast_impl!(!error, node, ctx, $fnarrow, $into),
                            LO::Warning => implicit_cast_impl!(!warning, node, ctx, {Some(i as f32)}, $fnarrow, $into),
                            LO::None => {Some(i as  f32)},
                        }
                    )?
                    $(
                        Constant::$fwide(i) => match cfg.widening {
                            LO::Error => implicit_cast_impl!(!error, node, ctx, $fwide, $into),
                            LO::Warning => implicit_cast_impl!(!warning, node, ctx, {Some(i as  f64)}, $fwide, $into),
                            LO::None => Some(i as f64),
                        }
                    )?
                    $(
                        Constant::$narrowing(i) => match cfg.narrowing {
                            LO::Error => implicit_cast_impl!(!error, node, ctx, $narrowing, $into),
                            LO::Warning => implicit_cast_impl!(!warning, node, ctx, i, cfg.lossy, $narrowing, $into),
                            LO::None => implicit_cast_impl!(!try_into, node, ctx, i, cfg.lossy, $narrowing, $into),
                        }
                    )*
                    $(
                        Constant::$widening(i) => match cfg.widening {
                            LO::Error => implicit_cast_impl!(!error, node, ctx, $widening, $into),
                            LO::Warning => implicit_cast_impl!(!warning, node, ctx, i, cfg.lossy, $widening, $into),
                            LO::None => implicit_cast_impl!(!try_into, node, ctx, i, cfg.lossy, $widening, $into),
                        }
                    )*
                    $(
                        Constant::$i2f(i) => match cfg.i2f {
                            LO::Error => implicit_cast_impl!(!error, node, ctx, $i2f, $into),
                            LO::Warning => implicit_cast_impl!(!warning, node, ctx, {Some(i as $into)}, $i2f, $into),
                            LO::None => Some(i as $into),
                        },
                    )*
                    $(
                        Constant::$f2i(i) => implicit_cast_impl!(!error, node, ctx, $f2i, $into),
                    )*
                    Constant::Char(_) => implicit_cast_impl!(!error, node, ctx, Char, $into),
                    Constant::Bool(_) => implicit_cast_impl!(!error, node, ctx, Bool, $into),
                    Constant::Str(_) => implicit_cast_impl!(!error_hard, node, ctx, Str, $into),
                    _ => todo!()
                }
            }
        }
    };
    (!error_hard, $node:ident, $ctx:ident, $from:ident, $to:ty) => {{
        let from = stringify!($from).to_lowercase();
        let to = stringify!($to).to_lowercase();
        $ctx.report(LogEntry::new().error($node, format!("cannot cast '{from}' to '{to}'")));
        None
    }};
    (!error, $node:ident, $ctx:ident, $from:ident, $to:ty) => {{
        let from = stringify!($from).to_lowercase();
        let to = stringify!($to).to_lowercase();
        $ctx.report(LogEntry::new().error($node, format!("cannot implicitly cast '{from}' to '{to}'")).hint_locless(format!("consider casting expression with 'as {to}'")));
        None
    }};
    (!warning, $node:ident, $ctx:ident, $block:block, $from:ident, $to:ty) => {{
        let from = stringify!($from).to_lowercase();
        let to = stringify!($to).to_lowercase();
        $ctx.report(LogEntry::new().warning($node, format!("implicit cast '{from}' to '{to}'")).hint_locless(format!("consider casting expression with 'as {to}'")));
        $block
    }};
    (!warning, $node:ident, $ctx:ident, $expr:expr, $lossy:expr, $from:ident, $to:ty) => {{
        implicit_cast_impl!(!warning, $node, $ctx, {
            implicit_cast_impl!(!try_into, $node, $ctx, $expr, $lossy, $from, $to)
        }, $from, $to)
    }};

    (!checked_cast, $node:ident, $ctx:ident, $expr:expr, $lossy:expr, $from:ident, $to:ty) => {{
        match $lossy{
            LO::Error => {
                match $expr.try_into(){
                    Ok(ok) => Some(ok),
                    Err(_) => implicit_cast_impl!(!error_hard, node, ctx, $from, $into)
                }
            },
            LO::Warning => {
                match $expr.try_into(){
                    Ok(ok) => Some(ok),
                    Err(_) => implicit_cast_impl!(!warning, $node, $ctx, {Some($expr as $to)}, $from, $to)
                }
            },
            LO::None => Some($expr as $to),
        }

    }};
    (!try_into, $node:ident, $ctx:ident, $expr:expr, $lossy:expr, $from:ident, $to:ty) => {{
        match $lossy{
            LO::Error => {
                match $expr.try_into(){
                    Ok(ok) => Some(ok),
                    Err(_) => {
                        let from = stringify!($from).to_lowercase();
                        let to = stringify!($to).to_lowercase();
                        $ctx.report(LogEntry::new().error($node, format!("implicit cast '{from}' to '{to}' is lossy")).hint_locless(format!("consider casting expression with 'as {to}'")));
                        None
                    }
                }
            },
            LO::Warning => {
                match $expr.try_into(){
                    Ok(ok) => Some(ok),
                    Err(_) => {
                        let from = stringify!($from).to_lowercase();
                        let to = stringify!($to).to_lowercase();
                        $ctx.report(LogEntry::new().error($node, format!("implicit cast '{from}' to '{to}' is lossy")).hint_locless(format!("consider casting expression with 'as {to}'")));
                        Some($expr as $to)
                    }
                }
            },
            LO::None => Some($expr as $to),
        }

    }};
}

implicit_cast_impl!(
    u8,
    identity: U8,
    identity_inv_sign: I8,
    narrowing: [ I16, I32, I64, I128, U16, U32, U64, U128],
    widening: [],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    u16,
    identity: U16,
    identity_inv_sign: I16,
    narrowing: [I32, I64, I128, U32, U64, U128],
    widening: [U8, I8],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    u32,
    identity: U32,
    identity_inv_sign: I32,
    narrowing: [I64, I128, U64, U128],
    widening: [U8, I8, I16, U16],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    u64,
    identity: U64,
    identity_inv_sign: I64,
    narrowing: [I128, U128],
    widening: [U8, I8, I16, U16, I32, U32],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    u128,
    identity: U128,
    identity_inv_sign: I128,
    narrowing: [],
    widening: [U8, I8, I16, U16, I32, U32, I64, U64],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i8,
    identity: I8,
    identity_inv_sign: U8,
    narrowing: [ I16, I32, I64, I128, U16, U32, U64, U128],
    widening: [],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i16,
    identity: I16,
    identity_inv_sign: U16,
    narrowing: [I32, I64, I128, U32, U64, U128],
    widening: [U8, I8],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i32,
    identity: I32,
    identity_inv_sign: U32,
    narrowing: [ I64, I128, U64, U128],
    widening: [U8, I8, I16, U16],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i64,
    identity: I64,
    identity_inv_sign: U64,
    narrowing: [I128, U128],
    widening: [U8, I8, I16, U16, I32, U32],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i128,
    identity: I128,
    identity_inv_sign: U128,
    narrowing: [],
    widening: [U8, I8, I16, U16, I32, U32, I64, U64],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    f32,
    identity: F32,
    fnarrow: F64,
    narrowing: [],
    widening: [],
    f2i: [],
    i2f: [U8, I8, I16, U16, I32, U32, I64, I128, U64, U128],
);
implicit_cast_impl!(
    f64,
    identity: F64,
    fwide: F32,
    narrowing: [],
    widening: [],
    f2i: [],
    i2f: [U8, I8, I16, U16, I32, U32, I64, I128, U64, U128],
);

impl<'a, L: AssemblyLanguage<'a>> ValueType<'a, L> {
    pub fn is_numeric(&self) -> bool {
        self.numeric_suffix().is_some()
    }
    pub fn is_integer(&self) -> bool {
        match self {
            ValueType::Any => false,
            ValueType::Register => false,
            ValueType::Indexed => false,
            ValueType::Str => false,
            ValueType::Label => false,
            ValueType::I8 => true,
            ValueType::I16 => true,
            ValueType::I32 => true,
            ValueType::I64 => true,
            ValueType::I128 => true,
            ValueType::Isize => true,
            ValueType::Iptr => true,
            ValueType::U8 => true,
            ValueType::U16 => true,
            ValueType::U32 => true,
            ValueType::U64 => true,
            ValueType::U128 => true,
            ValueType::Usize => true,
            ValueType::Uptr => true,
            ValueType::F32 => false,
            ValueType::F64 => false,
            ValueType::Bool => false,
            ValueType::Char => false,
            ValueType::Custom(_) => false,
            ValueType::Type => false,
        }
    }
    pub fn numeric_suffix(&self) -> Option<&'static str> {
        match self {
            ValueType::Any => None,
            ValueType::Register => None,
            ValueType::Indexed => None,
            ValueType::Str => None,
            ValueType::Label => None,
            ValueType::I8 => Some("i8"),
            ValueType::I16 => Some("i16"),
            ValueType::I32 => Some("i32"),
            ValueType::I64 => Some("i64"),
            ValueType::I128 => Some("i128"),
            ValueType::Isize => Some("isize"),
            ValueType::Iptr => Some("iptr"),
            ValueType::U8 => Some("u8"),
            ValueType::U16 => Some("u16"),
            ValueType::U32 => Some("u32"),
            ValueType::U64 => Some("u64"),
            ValueType::U128 => Some("u128"),
            ValueType::Usize => Some("usize"),
            ValueType::Uptr => Some("uptr"),
            ValueType::F32 => Some("f32"),
            ValueType::F64 => Some("f64"),
            ValueType::Bool => None,
            ValueType::Char => None,
            ValueType::Custom(_) => None,
            ValueType::Type => None,
        }
    }
}

//----------------------------------------------------------------------------

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ArgumentsTypeHint<'a, 'b, L: AssemblyLanguage<'a>> {
    Mono(ValueType<'a, L>),
    Comb(&'b [ValueType<'a, L>], ValueType<'a, L>),
    Individual(&'b [ValueType<'a, L>]),
    None,
}

impl<'a, 'b, L: AssemblyLanguage<'a>> Index<usize> for ArgumentsTypeHint<'a, 'b, L> {
    type Output = ValueType<'a, L>;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            ArgumentsTypeHint::Mono(ty) => ty,
            ArgumentsTypeHint::Individual(ty) => ty.get(index).unwrap_or(&ValueType::Any),
            ArgumentsTypeHint::None => &ValueType::Any,
            ArgumentsTypeHint::Comb(value_types, value_type) => {
                if let Some(hint) = value_types.get(index) {
                    hint
                } else {
                    value_type
                }
            }
        }
    }
}
