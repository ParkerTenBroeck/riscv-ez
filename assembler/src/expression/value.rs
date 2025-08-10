use std::convert::Infallible;
use std::fmt::{Debug, Display, Formatter};
use std::marker::PhantomData;
use std::ops::Index;

use crate::assembler::lang::AssemblyLanguage;
use crate::config::ImplicitCastConfig;
use crate::context::{Context, NodeId};
use crate::expression::AsmStr;

pub trait AssemblyLabel<'a>:
    Sized + Default + std::fmt::Display + std::fmt::Debug + Copy + Clone + Eq + PartialEq
{
    type Lang: AssemblyLanguage<'a>;
    type Offset: ImplicitCastFrom<'a, Constant<'a>> + Default;
}

pub trait AssemblyRegister<'a>:
    Debug + Clone + Copy + PartialEq + Display + Default + Sized
{
    type Lang: AssemblyLanguage<'a>;
}
pub trait Indexed<'a>: Debug + Clone + Copy + PartialEq + Display + Default + Sized {
    type Lang: AssemblyLanguage<'a>;
}

pub trait CustomValue<'a>: Debug + Clone + Copy + PartialEq + Eq + Display + Sized {
    type Lang: AssemblyLanguage<'a>;
    type CustomValueType: CustomValueType<'a, CustomValue = Self>;
    fn get_align(&self) -> Option<u32>;
    fn get_size(&self) -> Option<u32>;
    fn get_type(&self) -> Self::CustomValueType;
}
pub trait CustomValueType<'a>: Debug + Clone + Copy + PartialEq + Eq + Display + Sized {
    type Lang: AssemblyLanguage<'a>;
    type CustomValue: CustomValue<'a, CustomValueType = Self>;
    fn default_value(&self) -> Self::CustomValue;
}

//----------------------------------------------------------------------------

#[derive(Debug, Eq)]
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

    U8,
    U16,
    U32,
    U64,

    F32,
    F64,

    Bool,
    Char,
    Custom(<L::CustomValue as CustomValue<'a>>::CustomValueType),
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

impl<'a, L: AssemblyLanguage<'a>> Clone for ValueType<'a, L> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, L: AssemblyLanguage<'a>> Copy for ValueType<'a, L> {}

impl<'a, L: AssemblyLanguage<'a>> Display for ValueType<'a, L> {
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
            ValueType::Str => Value::Constant(Constant::Str(Default::default())),
            ValueType::Indexed => Value::Indexed(L::Indexed::default()),
            ValueType::Register => Value::Register(L::Reg::default()),
            ValueType::Label => Value::Label(Default::default()),
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
    fn get_align(&self) -> Option<u32> {
        unreachable!()
    }

    fn get_size(&self) -> Option<u32> {
        unreachable!()
    }

    fn get_type(&self) -> Self::CustomValueType {
        unreachable!()
    }
}

//----------------------------------------------------------------------------

pub enum Value<'a, L: AssemblyLanguage<'a>> {
    Constant(Constant<'a>),
    Label(L::Label),
    Indexed(L::Indexed),
    Register(L::Reg),
    Custom(L::CustomValue),
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
            Value::Constant(c) => c.get_type(),
            Value::Label(_) => ValueType::Label,
            Value::Indexed(_) => ValueType::Indexed,
            Value::Register(_) => ValueType::Register,
            Value::Custom(c) => ValueType::Custom(c.get_type()),
        }
    }

    pub fn get_size(&self) -> Option<u32> {
        match *self {
            Value::Constant(c) => Some(c.get_size()),
            Value::Label(_) => Some(4),
            Value::Indexed(_) => None,
            Value::Register(_) => None,
            Value::Custom(c) => c.get_size(),
        }
    }

    pub fn get_align(&self) -> Option<u32> {
        match *self {
            Value::Constant(c) => Some(c.get_align()),
            Value::Label(_) => Some(4),
            Value::Indexed(_) => None,
            Value::Register(_) => None,
            Value::Custom(c) => c.get_align(),
        }
    }

    pub fn is_true(&self) -> bool {
        matches!(self, Value::Constant(Constant::Bool(true)))
    }
}

//----------------------------------------------------------------------------

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

    Str(AsmStr<'a>),
    Char(char),
    Bool(bool),
}

impl<'a> Constant<'a> {
    pub fn is_float(&self) -> bool {
        matches!(self, Self::F32(_) | Self::F64(_))
    }

    pub fn get_type<L: AssemblyLanguage<'a>>(&self) -> ValueType<'a, L> {
        match self {
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

    pub fn get_align(&self) -> u32 {
        match self {
            Constant::I8(_) => 1,
            Constant::I16(_) => 2,
            Constant::I32(_) => 4,
            Constant::I64(_) => 8,
            Constant::U8(_) => 1,
            Constant::U16(_) => 2,
            Constant::U32(_) => 4,
            Constant::U64(_) => 8,
            Constant::F32(_) => 4,
            Constant::F64(_) => 8,
            Constant::Str(_) => 1,
            Constant::Char(_) => 4,
            Constant::Bool(_) => 1,
        }
    }

    pub fn get_size(&self) -> u32 {
        match self {
            Constant::I8(_) => 1,
            Constant::I16(_) => 2,
            Constant::I32(_) => 4,
            Constant::I64(_) => 8,
            Constant::U8(_) => 1,
            Constant::U16(_) => 2,
            Constant::U32(_) => 4,
            Constant::U64(_) => 8,
            Constant::F32(_) => 4,
            Constant::F64(_) => 8,
            Constant::Str(str) => str.len() as u32,
            Constant::Char(_) => 4,
            Constant::Bool(_) => 1,
        }
    }
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
            Constant::Str(c) => write!(f, "{c}"),
            Constant::Char(c) => write!(f, "{c}"),
            Constant::Bool(c) => write!(f, "{c}"),
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
        impl<'a> ImplicitCastFrom<'a, Constant<'a>> for $into {
            fn cast_with(
                from: Constant<'a>,
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
    narrowing: [ I16, I32, I64, U16, U32, U64],
    widening: [],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    u16,
    identity: U16,
    identity_inv_sign: I16,
    narrowing: [I32, I64, U32, U64],
    widening: [U8, I8],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    u32,
    identity: U32,
    identity_inv_sign: I32,
    narrowing: [ I64, U64],
    widening: [U8, I8, I16, U16],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    u64,
    identity: U64,
    identity_inv_sign: I64,
    narrowing: [],
    widening: [U8, I8, I16, U16, I32, U32],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i8,
    identity: I8,
    identity_inv_sign: U8,
    narrowing: [ I16, I32, I64, U16, U32, U64],
    widening: [],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i16,
    identity: I16,
    identity_inv_sign: U16,
    narrowing: [I32, I64, U32, U64],
    widening: [U8, I8],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i32,
    identity: I32,
    identity_inv_sign: U32,
    narrowing: [ I64, U64],
    widening: [U8, I8, I16, U16],
    f2i: [],
    i2f: [F32, F64],
);
implicit_cast_impl!(
    i64,
    identity: I64,
    identity_inv_sign: U64,
    narrowing: [],
    widening: [U8, I8, I16, U16, I32, U32],
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
    i2f: [U8, I8, I16, U16, I32, U32, I64, U64],
);
implicit_cast_impl!(
    f64,
    identity: F64,
    fwide: F32,
    narrowing: [],
    widening: [],
    f2i: [],
    i2f: [U8, I8, I16, U16, I32, U32, I64, U64],
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
            ValueType::Str => None,
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
