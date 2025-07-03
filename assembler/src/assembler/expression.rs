use std::fmt::{format, Display, Formatter};
use crate::assembler::Assembler;
use crate::assembler::instructions::Register;
use crate::context::{Node, NodeId};
use crate::lex::{Number, Token};
use std::ops::Index;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ExpressionType {
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

impl Display for ExpressionType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self{
            ExpressionType::Any => write!(f, "any"),
            ExpressionType::String => write!(f, "string"),
            ExpressionType::Indexed => write!(f, "indexed"),
            ExpressionType::Register => write!(f, "register"),
            ExpressionType::Label => write!(f, "label"),
            ExpressionType::I8 => write!(f, "i8"),
            ExpressionType::I16 => write!(f, "i16"),
            ExpressionType::I32 => write!(f, "i32"),
            ExpressionType::I64 => write!(f, "i64"),
            ExpressionType::U8 => write!(f, "u8"),
            ExpressionType::U16 => write!(f, "u16"),
            ExpressionType::U32 => write!(f, "u32"),
            ExpressionType::U64 => write!(f, "u64"),
            ExpressionType::F32 => write!(f, "f32"),
            ExpressionType::F64 => write!(f, "f64"),
            ExpressionType::Bool => write!(f, "bool"),
            ExpressionType::Char => write!(f, "char"),
        }
    }
}

impl ExpressionType {
    pub fn default_value<'a>(&self) -> Value<'a> {
        match self {
            ExpressionType::Any => Value::Constant(Constant::I32(0)),
            ExpressionType::String => Value::Constant(Constant::String("")),
            ExpressionType::Indexed => Value::LabelRegisterOffset(Register(0), LabelUse{
                ident: "",
                offset: 0,
                meta: LabelMeta::PcRel,
            }),
            ExpressionType::Register => Value::Register(Register(0)),
            ExpressionType::Label => Value::Label(LabelUse{
                ident: "",
                offset: 0,
                meta: LabelMeta::PcRel,
            }),
            ExpressionType::I8 => Value::Constant(Constant::I8(0)),
            ExpressionType::I16 => Value::Constant(Constant::I16(0)),
            ExpressionType::I32 => Value::Constant(Constant::I32(0)),
            ExpressionType::I64 => Value::Constant(Constant::I16(0)),
            ExpressionType::U8 => Value::Constant(Constant::U8(0)),
            ExpressionType::U16 => Value::Constant(Constant::U16(0)),
            ExpressionType::U32 => Value::Constant(Constant::U32(0)),
            ExpressionType::U64 => Value::Constant(Constant::U64(0)),
            ExpressionType::F32 => Value::Constant(Constant::F32(0.0)),
            ExpressionType::F64 => Value::Constant(Constant::F64(0.0)),
            ExpressionType::Bool => Value::Constant(Constant::Bool(false)),
            ExpressionType::Char => Value::Constant(Constant::Char('\0')),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    Xor,
    Or,
    Shl,
    Shr,

    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Ne,
}

impl BinOp {
    pub fn precedence(&self) -> u32 {
        match self {
            BinOp::Add => 20 - 4,
            BinOp::Sub => 20 - 4,

            BinOp::Mul => 20 - 3,
            BinOp::Div => 20 - 3,
            BinOp::Rem => 20 - 3,

            BinOp::Shl => 20 - 5,
            BinOp::Shr => 20 - 5,

            BinOp::Lt => 20 - 6,
            BinOp::Lte => 20 - 6,
            BinOp::Gt => 20 - 6,
            BinOp::Gte => 20 - 6,

            BinOp::Eq => 20 - 7,
            BinOp::Ne => 20 - 7,

            BinOp::And => 20 - 8,
            BinOp::Xor => 20 - 9,
            BinOp::Or => 20 - 10,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum LabelMeta{
    PcRel,
    Absolute,
    Size,
    Unset
}

impl LabelMeta {
    pub fn is_unset(&self) -> bool {
        matches!(self, LabelMeta::Unset)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Expression<'a> {
    pub node: NodeId<'a>,
    pub value: Value<'a>,
}

#[derive(Debug, Clone, Copy)]
pub struct LabelUse<'a>{
    ident: &'a str,
    offset: i32,
    meta: LabelMeta,
}

impl<'a> LabelUse<'a> {
    fn new(ident: &'a str) -> LabelUse {
        LabelUse{
            ident,
            offset: 0,
            meta: LabelMeta::Unset,
        }
    }
}

impl<'a> Display for LabelUse<'a>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.meta {
            LabelMeta::PcRel => write!(f, "pc_rel(")?,
            LabelMeta::Absolute => write!(f, "absolute(")?,
            LabelMeta::Size => write!(f, "size(")?,
            LabelMeta::Unset => {}
        }
        write!(f, "{}", self.ident)?;
        if self.offset != 0 {
            write!(f, "+{}", self.offset)?;
        }
        if !self.meta.is_unset(){
            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Value<'a>{
    Constant(Constant<'a>),
    Label(LabelUse<'a>),
    LabelRegisterOffset(Register, LabelUse<'a>),
    RegisterOffset(Register, i32),
    Register(Register),
}

impl<'a> Display for Value<'a>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Value::Constant(c) => write!(f, "{}", c),
            Value::Label(l) => write!(f, "{}", l),
            Value::LabelRegisterOffset(reg, l) => {
                if l.meta.is_unset() && l.offset != 0 {
                    write!(f, "(")?;
                }
                write!(f, "{}", l)?;
                if l.meta.is_unset() && l.offset != 0{
                    write!(f, ")")?;
                }
                if reg.0 != 0{
                    write!(f, "[{reg}]")?;
                }
                Ok(())
            }
            Value::RegisterOffset(reg, offset) => {
                if reg.0 == 0{
                    write!(f, "{offset}")?;
                }else{
                    write!(f, "{reg}")?;
                    if offset == 0 {
                        write!(f, "[{offset}]")?;
                    } 
                }
                Ok(())
            }
            Value::Register(reg) => write!(f, "{}", reg),
        }
    }
}

impl<'a> Value<'a>{
    pub fn get_type(&self) -> ExpressionType{
        match self{
            Value::Constant(c) => match c{
                Constant::I8(_) => ExpressionType::I8,
                Constant::I16(_) => ExpressionType::I16,
                Constant::I32(_) => ExpressionType::I32,
                Constant::I64(_) => ExpressionType::I64,
                Constant::U8(_) => ExpressionType::U8,
                Constant::U16(_) => ExpressionType::U16,
                Constant::U32(_) => ExpressionType::U32,
                Constant::U64(_) => ExpressionType::U64,
                Constant::F32(_) => ExpressionType::F32,
                Constant::F64(_) => ExpressionType::F64,
                Constant::String(_) => ExpressionType::String,
                Constant::Char(_) => ExpressionType::Char,
                Constant::Bool(_) => ExpressionType::Bool,
            }
            Value::Label(_) => ExpressionType::Label,
            Value::LabelRegisterOffset(_, _) => ExpressionType::Indexed,
            Value::RegisterOffset(_, _) => ExpressionType::Indexed,
            Value::Register(_) => ExpressionType::Register,
        }
    }
    
    pub fn get_size(&self) -> Option<u32>{
        match *self{
            Value::Constant(c) => match c{
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

    pub fn get_align(&self) -> Option<u32>{
        match *self{
            Value::Constant(c) => match c{
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

#[derive(Clone, Copy, Debug)]
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

impl<'a> Display for Constant<'a>{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self{
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

impl<'a> Constant<'a> {
    pub fn to_u32(&self) -> ConvertResult<u32> {
        match *self {
            Constant::I8(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I8(v) => ConvertResult::Success(v as u32),
            Constant::I16(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I16(v) => ConvertResult::Success(v as u32),
            Constant::I32(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I32(v) => ConvertResult::Success(v as u32),
            Constant::I64(v) if v > u32::MAX as i64 => ConvertResult::Lossy(v as u32),
            Constant::I64(v) if v < 0 => ConvertResult::Lossy(v as u32),
            Constant::I64(v) => ConvertResult::Success(v as u32),
            Constant::U8(v) => ConvertResult::Success(v as u32),
            Constant::U16(v) => ConvertResult::Success(v as u32),
            Constant::U32(v) => ConvertResult::Success(v),
            Constant::U64(v) if v > u32::MAX as u64 => ConvertResult::Lossy(v as u32),
            Constant::U64(v) => ConvertResult::Success(v as u32),
            Constant::F32(_) => ConvertResult::Failure,
            Constant::F64(_) => ConvertResult::Failure,
            Constant::String(_) => ConvertResult::Failure,
            Constant::Char(char) => ConvertResult::Success(char as u32),
            Constant::Bool(bool) => ConvertResult::Success(bool as u32),
        }
    }
}

impl ExpressionType {
    pub fn size(&self) -> Option<u32> {
        match self {
            ExpressionType::Any => None,
            ExpressionType::Indexed => None,
            ExpressionType::Register => None,
            ExpressionType::String => None,
            ExpressionType::Label => Some(4),
            ExpressionType::I8 => Some(1),
            ExpressionType::I16 => Some(2),
            ExpressionType::I32 => Some(4),
            ExpressionType::I64 => Some(8),
            ExpressionType::U8 => Some(1),
            ExpressionType::U16 => Some(2),
            ExpressionType::U32 => Some(4),
            ExpressionType::U64 => Some(8),
            ExpressionType::F32 => Some(4),
            ExpressionType::F64 => Some(8),
            ExpressionType::Bool => Some(1),
            ExpressionType::Char => Some(4),
        }
    }

    pub fn align(&self) -> u32 {
        match self {
            ExpressionType::Any => 0,
            ExpressionType::Indexed => 0,
            ExpressionType::Register => 0,
            ExpressionType::String => 1,
            ExpressionType::Label => 4,
            ExpressionType::I8 => 1,
            ExpressionType::I16 => 2,
            ExpressionType::I32 => 4,
            ExpressionType::I64 => 8,
            ExpressionType::U8 => 1,
            ExpressionType::U16 => 2,
            ExpressionType::U32 => 4,
            ExpressionType::U64 => 8,
            ExpressionType::F32 => 4,
            ExpressionType::F64 => 8,
            ExpressionType::Bool => 1,
            ExpressionType::Char => 4,
        }
    }

    pub fn numeric_suffix(&self) -> Option<&'static str> {
        match self {
            ExpressionType::Any => None,
            ExpressionType::Register => None,
            ExpressionType::Indexed => None,
            ExpressionType::String => None,
            ExpressionType::Label => None,
            ExpressionType::I8 => Some("i8"),
            ExpressionType::I16 => Some("i16"),
            ExpressionType::I32 => Some("i32"),
            ExpressionType::I64 => Some("i64"),
            ExpressionType::U8 => Some("u8"),
            ExpressionType::U16 => Some("u16"),
            ExpressionType::U32 => Some("u32"),
            ExpressionType::U64 => Some("u64"),
            ExpressionType::F32 => Some("f32"),
            ExpressionType::F64 => Some("f64"),
            ExpressionType::Bool => None,
            ExpressionType::Char => None,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            ExpressionType::Any => false,
            ExpressionType::Register => false,
            ExpressionType::Indexed => false,
            ExpressionType::String => false,
            ExpressionType::Label => false,
            ExpressionType::I8 => true,
            ExpressionType::I16 => true,
            ExpressionType::I32 => true,
            ExpressionType::I64 => true,
            ExpressionType::U8 => true,
            ExpressionType::U16 => true,
            ExpressionType::U32 => true,
            ExpressionType::U64 => true,
            ExpressionType::F32 => false,
            ExpressionType::F64 => false,
            ExpressionType::Bool => false,
            ExpressionType::Char => false,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ArgumentsTypeHint<'a> {
    Mono(ExpressionType),
    Individual(&'a [ExpressionType]),
    None,
}

impl<'a> Index<usize> for ArgumentsTypeHint<'a> {
    type Output = ExpressionType;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            ArgumentsTypeHint::Mono(ty) => ty,
            ArgumentsTypeHint::Individual(ty) => ty.get(index).unwrap_or(&ExpressionType::Any),
            ArgumentsTypeHint::None => &ExpressionType::Any,
        }
    }
}

impl<'a> Assembler<'a> {
    fn parse_numeric_literal(
        &mut self,
        num: Number<'a>,
        n: NodeId<'a>,
        hint: ExpressionType,
    ) -> Constant<'a> {
        let (suffix, radix) = match num.get_hint() {
            crate::lex::TypeHint::Float if hint.is_integer() => {
                (num.get_suffix().unwrap_or("f32"), 10)
            }
            crate::lex::TypeHint::Float => (
                num.get_suffix()
                    .unwrap_or(hint.numeric_suffix().unwrap_or("f32")),
                10,
            ),
            crate::lex::TypeHint::Hex => (
                num.get_suffix().unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or("i32"),
                ),
                16,
            ),
            crate::lex::TypeHint::Bin => (
                num.get_suffix().unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or("i32"),
                ),
                2,
            ),
            crate::lex::TypeHint::Int => (
                num.get_suffix().unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or("i32"),
                ),
                10,
            ),
            crate::lex::TypeHint::Oct => (
                num.get_suffix().unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or("i32"),
                ),
                8,
            ),
        };

        macro_rules! integer {
            ($num:ty) => {
                <$num>::from_str_radix(num.get_num(), radix)
                    .inspect_err(|e| {
                        self.context
                            .context
                            .report_error(n, format!("Invalid numeric literal {e}"));
                    })
                    .unwrap_or(0)
            };
        }

        macro_rules! float {
            ($num:ty) => {
                num.get_num()
                    .parse()
                    .inspect_err(|e| {
                        self.context
                            .context
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
                self.context
                    .context
                    .report_error(n, format!("Unknown numeric suffix '{suffix}'"));
                if hint.numeric_suffix().is_some() {
                    if let Value::Constant(c) = hint.default_value() {
                        c
                    } else {
                        Constant::I32(0)
                    }
                } else {
                    Constant::I32(0)
                }
            }
        }
    }

    fn parse_expr_1(&mut self, hint: ExpressionType) -> Expression<'a> {
        let expr = match self.next() {
            Some(Node(Token::Ident(ident), node)) => match self.peek() {
                Some(Node(Token::LPar, lhs)) => {
                    self.next();
                    let args = self.parse_arguments(ArgumentsTypeHint::None, Some(Token::RPar));
                    let rhs = match self.peek() {
                        Some(Node(Token::RPar, node)) => {
                            self.next();
                            node
                        }
                        Some(Node(t, node)) => {
                            self.context.context.report_error(
                                node,
                                format!("Unexpected token '{t:?}' expected ')'"),
                            );
                            lhs
                        }
                        None => {
                            self.context
                                .context
                                .report_error_eof("Expected ')' but found eof");
                            self.context.context.top_src_eof()
                        }
                    };
                    let args_node = self.context.context.merge_nodes(lhs, rhs);
                    self.func(ident, node, args, args_node)
                }
                _ => self.handle_ident(ident, node, hint),
            },
            Some(Node(Token::TrueLiteral, node)) => Expression {
                value: Value::Constant(Constant::Bool(true)),
                node,
            },
            Some(Node(Token::FalseLiteral, node)) => Expression {
                value: Value::Constant(Constant::Bool(false)),
                node,
            },
            Some(Node(Token::StringLiteral(str), node)) => Expression {
                value: Value::Constant(Constant::String(
                    self.parse_string_literal(str, node),
                )),
                node,
            },
            Some(Node(Token::CharLiteral(str), node)) => Expression {
                value: Value::Constant(Constant::Char(self.parse_char_literal(str, node))),
                node,
            },
            Some(Node(Token::NumericLiteral(num), node)) => Expression {
                value: Value::Constant(self.parse_numeric_literal(num, node, hint)),
                node,
            },
            Some(Node(Token::LPar, lhs)) => {
                let mut arg = self.parse_expr(hint);
                match self.peek() {
                    Some(Node(Token::RPar, rhs)) => {
                        arg.node = self.context.context.merge_nodes(lhs, rhs);
                        self.next();
                    }
                    Some(Node(t, rhs)) => {
                        self.context
                            .context
                            .report_error(rhs, format!("Unexpected token '{t:?}' expected ')'"));
                        arg.node = self.context.context.merge_nodes(lhs, rhs);
                    }
                    None => {
                        self.context
                            .context
                            .report_error_eof("Expected ')' but found eof");
                    }
                }
                arg
            }

            Some(Node(t, node)) => {
                self.context
                    .context
                    .report_error(node, format!("Unexpected token '{t:?}'"));
                Expression{
                    value: hint.default_value(),
                    node,
                }
            }
            None => {
                self.context
                    .context
                    .report_error_eof("Expected token but found eof");
                Expression{
                    value: hint.default_value(),
                    node: self.context.context.top_src_eof(),
                }
            }
        };

        match self.peek() {
            Some(Node(Token::LBracket, _)) => {
                self.next();
                let rhs = self.parse_expr(hint);

                let node = match self.peek() {
                    Some(Node(Token::RBracket, node)) => {
                        self.next();
                        node
                    }
                    Some(Node(t, node)) => {
                        self.context
                            .context
                            .report_error(node, format!("Unexpected token '{t:?}' expected ']'"));
                        expr.node
                    }
                    None => {
                        self.context
                            .context
                            .report_error_eof("Expected ']' but found eof");
                        self.context.context.top_src_eof()
                    }
                };
                return self.index(expr, rhs, node);
            }
            Some(Node(Token::Ident("as"), _)) => {
                self.next();

                let (node, ty) = match self.peek() {
                    Some(Node(Token::Ident(ty), node)) => {
                        self.next();
                        (node, ty)
                    }
                    Some(Node(t, node)) => {
                        self.context.context.report_error(
                            node,
                            format!("Unexpected token '{t:?}' expected identifier"),
                        );
                        (expr.node, "i32")
                    }
                    None => {
                        self.context
                            .context
                            .report_error_eof("Expected identifier but found eof");
                        (self.context.context.top_src_eof(), "i32")
                    }
                };
                return self.cast(expr, ty, node);
            }
            _ => {}
        }

        expr
    }

    fn parse_expr_2(&mut self, hint: ExpressionType, min_prec: u32) -> Expression<'a> {
        let mut lhs = self.parse_expr_1(hint);
        loop{
            let op = match self.peek(){
                Some(Node(Token::Plus, _)) if BinOp::Add.precedence() >= min_prec => BinOp::Add,
                Some(Node(Token::Minus, _)) if BinOp::Sub.precedence() >= min_prec => BinOp::Sub,

                Some(Node(Token::Star, _)) if BinOp::Mul.precedence() >= min_prec => BinOp::Mul,
                Some(Node(Token::Slash, _)) if BinOp::Div.precedence() >= min_prec => BinOp::Div,
                Some(Node(Token::Percent, _)) if BinOp::Rem.precedence() >= min_prec => BinOp::Rem,

                Some(Node(Token::LogicalOr, _)) if BinOp::Or.precedence() >= min_prec => BinOp::Or,
                Some(Node(Token::BitwiseXor, _)) if BinOp::Or.precedence() >= min_prec => BinOp::Or,

                Some(Node(Token::Ampersand, _)) if BinOp::And.precedence() >= min_prec => BinOp::And,
                Some(Node(Token::LogicalAnd, _)) if BinOp::And.precedence() >= min_prec => BinOp::And,

                Some(Node(Token::BitwiseXor, _)) if BinOp::Xor.precedence() >= min_prec => BinOp::Xor,

                Some(Node(Token::ShiftLeft, _)) if BinOp::Shl.precedence() >= min_prec => BinOp::Shl,
                Some(Node(Token::ShiftRight, _)) if BinOp::Shr.precedence() >= min_prec => BinOp::Shr,

                Some(Node(Token::GreaterThan, _)) if BinOp::Gt.precedence() >= min_prec => BinOp::Gt,
                Some(Node(Token::GreaterThanEq, _)) if BinOp::Gte.precedence() >= min_prec => BinOp::Gte,
                Some(Node(Token::LessThan, _)) if BinOp::Lt.precedence() >= min_prec => BinOp::Lt,
                Some(Node(Token::LessThanEq, _)) if BinOp::Lte.precedence() >= min_prec => BinOp::Lte,
                Some(Node(Token::Equals, _)) if BinOp::Eq.precedence() >= min_prec => BinOp::Eq,
                Some(Node(Token::NotEquals, _)) if BinOp::Ne.precedence() >= min_prec => BinOp::Ne,
                _ => break,
            };
            self.next();

            let rhs = self.parse_expr_2(hint, op.precedence()+1);
            lhs = self.binop(op, lhs, rhs);
        };
        lhs
    }

    fn parse_expr_3(&mut self, hint: ExpressionType) -> Expression<'a> {
        match self.peek() {
            Some(Node(Token::Minus, node)) => {
                self.next();
                let expr = self.parse_expr_3(hint);
                self.unop(UnOp::Neg, node, expr)
            }
            Some(Node(Token::LogicalNot, node)) => {
                self.next();
                let expr = self.parse_expr_3(hint);
                self.unop(UnOp::Not, node, expr)
            }
            _ => self.parse_expr_2(hint, 0),
        }
    }

    fn parse_expr(&mut self, hint: ExpressionType) -> Expression<'a> {
        self.parse_expr_3(hint)
    }

    fn parse_char_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> char {
        let mut chars = repr.chars();
        if let Some(ok) = chars.next() {
            if chars.next().is_some() {
                self.context
                    .context
                    .report_error(n, "Char literal contains more than one char");
            }
            ok
        } else {
            self.context.context.report_error(n, "Char literal empty");
            '\0'
        }
    }

    fn parse_string_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> &'a str {
        repr.into()
    }

    pub(super) fn parse_arguments(
        &mut self,
        hint: ArgumentsTypeHint<'_>,
        closing: Option<Token<'a>>,
    ) -> Vec<Expression<'a>> {
        let mut args = Vec::new();
        match self.peek() {
            Some(Node(Token::NewLine, _)) | None if closing.is_none() => return args,
            Some(Node(t, _)) if Some(t) == closing => return args,
            _ => {}
        }
        loop {
            args.push(self.parse_expr(hint[args.len()]));
            match self.peek() {
                Some(Node(Token::Comma, _)) => {
                    self.next();
                }
                Some(Node(Token::NewLine, _)) | None if closing.is_none() => return args,
                Some(Node(t, _)) if Some(t) == closing => return args,
                Some(Node(t, n)) => {
                    self.context
                        .context
                        .report_error(n, format!("Expected comma found '{t:?}'"));
                }
                None => {
                    self.context
                        .context
                        .report_error_eof("Expected comma found eof");
                    return args;
                }
            }
        }
    }

    fn handle_ident(
        &mut self,
        ident: &'a str,
        node: NodeId<'a>,
        hint: ExpressionType,
    ) -> Expression<'a> {
        fn reg(node: NodeId, reg: u8) -> Expression {
            Expression {
                node,
                value: Value::Register(Register(reg)),
            }
        }
        match ident {
            "x0" | "zero" => reg(node, 0),
            "x1" | "ra" => reg(node, 1),
            "x2" | "sp" => reg(node, 2),
            "x3" | "gp" => reg(node, 3),
            "x4" | "tp" => reg(node, 4),
            "x5" | "t0" => reg(node, 5),
            "x6" | "t1" => reg(node, 6),
            "x7" | "t2" => reg(node, 7),
            "x8" | "s0" | "fp" => reg(node, 8),
            "x9" | "s1" => reg(node, 9),
            "x10" | "a0" => reg(node, 10),
            "x11" | "a1" => reg(node, 11),
            "x12" | "a2" => reg(node, 12),
            "x13" | "a3" => reg(node, 13),
            "x14" | "a4" => reg(node, 14),
            "x15" | "a5" => reg(node, 15),
            "x16" | "a6" => reg(node, 16),
            "x17" | "a7" => reg(node, 17),
            "x18" | "s2" => reg(node, 18),
            "x19" | "s3" => reg(node, 19),
            "x20" | "s4" => reg(node, 20),
            "x21" | "s5" => reg(node, 21),
            "x22" | "s6" => reg(node, 22),
            "x23" | "s7" => reg(node, 23),
            "x24" | "s8" => reg(node, 24),
            "x25" | "s9" => reg(node, 25),
            "x26" | "s10" => reg(node, 26),
            "x27" | "s11" => reg(node, 27),
            "x28" | "t3" => reg(node, 28),
            "x29" | "t4" => reg(node, 29),
            "x30" | "t5" => reg(node, 30),
            "x31" | "t6" => reg(node, 31),

            _ => {
                Expression{
                    value: Value::Label(LabelUse::new(ident)),
                    node
                }
            }
        }
    }

    fn func(
        &mut self,
        func: &'a str,
        func_node: NodeId<'a>,
        args: Vec<Expression<'a>>,
        args_node: NodeId<'a>,
    ) -> Expression<'a> {
        let node = self.context.context.merge_nodes(func_node, args_node);
        let value = match func{
            "size" => Value::Constant(Constant::I32(0)),
            "align" => Value::Constant(Constant::I32(1)),
            "pcrel" => Value::Constant(Constant::I32(0)),
            "absolute" => Value::Constant(Constant::I32(0)),
            "format" => Value::Constant(Constant::String("nyaaa~")),
            _ => {
                self.context.context.report_error(func_node, format!("Unknown function {}", func));
                Value::Constant(Constant::I32(0))
            }
        };
        Expression {
            node,
            value,
        }
    }

    fn index(
        &mut self,
        lhs: Expression<'a>,
        rhs: Expression<'a>,
        closing: NodeId<'a>,
    ) -> Expression<'a> {
        let node = self.context.context.merge_nodes(lhs.node, closing);
        let value = Value::Constant(Constant::I32(0));
        self.context.context.report_error(node, format!("Cannot index {} with {}", lhs.value.get_type(), rhs.value.get_type()));
        Expression {
            node,
            value,
        }
    }

    fn cast_error(&mut self, expr: Expression<'a>, expected: ExpressionType) -> Value<'a> {
        self.context.context.report_error(expr.node, format!("Cannot cast {} to {}", expr.value.get_type(), expected));
        expected.default_value()
    }

    fn cast(&mut self, expr: Expression<'a>, ty: &'a str, node_id: NodeId<'a>) -> Expression<'a> {
        
        macro_rules! integer {
            ($ident:ident, $ty:ty) => {
                match expr.value{
                    Value::Constant(Constant::I8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I16(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I32(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I64(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U16(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U32(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U64(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::F32(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::F64(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::Bool(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::Char(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    _ => self.cast_error(expr, ExpressionType::$ident),
                }
            };
        }

        macro_rules! float {
            ($ident:ident, $ty:ty) => {
                match expr.value{
                    Value::Constant(Constant::I8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I16(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I32(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I64(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U16(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U32(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U64(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::F32(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::F64(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    _ => self.cast_error(expr, ExpressionType::$ident),
                }
            };
        }
        
        let node = self.context.context.merge_nodes(expr.node, node_id);
        let value = match ty{
            "str" => Value::Constant(Constant::String(self.context.context.alloc_str(format!("{}", expr.value).as_str()))),
            "i8" => integer!(I8, i8),
            "i16" => integer!(I16, i16),
            "i32" => integer!(I32, i32),
            "i64" => integer!(I64, i64),
            "u8" => integer!(U8, u8),
            "u16" => integer!(U16, u16),
            "u32" => integer!(U32, u32),
            "u64" => integer!(U64, u64),
            "f32" => float!(F32, f32),
            "f64" => float!(F64, f64),
            "char" => match expr.value{
                Value::Constant(Constant::U8(i)) => Value::Constant(Constant::Char(i as char)),
                Value::Constant(Constant::U16(i)) => Value::Constant(Constant::Char(char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER))),
                Value::Constant(Constant::U32(i)) => Value::Constant(Constant::Char(char::from_u32(i).unwrap_or(char::REPLACEMENT_CHARACTER))),
                Value::Constant(Constant::U64(i)) => Value::Constant(Constant::Char(char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER))),
                Value::Constant(Constant::Char(i)) => Value::Constant(Constant::Char(i)),
                _ => self.cast_error(expr, ExpressionType::Char),
            },
            _ => {
                self.context.context.report_error(node, format!("Unknown type {}", ty));
                expr.value
            }
        };
        Expression {
            node,
            value,
        }
    }

    fn unop(&mut self, op: UnOp, node: NodeId<'a>, mut expr: Expression<'a>) -> Expression<'a> {
        let node = self.context.context.merge_nodes(node, expr.node);
        match op{
            UnOp::Neg => match &mut expr.value{
                Value::Constant(c) => match c{
                    Constant::I8(i) => *i = -*i,
                    Constant::I16(i) => *i = -*i,
                    Constant::I32(i) => *i = -*i,
                    Constant::I64(i) => *i = -*i,
                    Constant::F32(i) => *i = -*i,
                    Constant::F64(i) => *i = -*i,
                    _ => self.context.context.report_error(node, format!("Cannot negate expression of type {}", expr.value.get_type()))
                }
                _ => self.context.context.report_error(node, format!("Cannot negate expression of type {}", expr.value.get_type()))
            }
            UnOp::Not => match &mut expr.value{
                Value::Constant(c) => match c{
                    Constant::I8(i) => *i = !*i,
                    Constant::I16(i) => *i = !*i,
                    Constant::I32(i) => *i = !*i,
                    Constant::I64(i) => *i = !*i,
                    Constant::U8(i) => *i = !*i,
                    Constant::U16(i) => *i = !*i,
                    Constant::U32(i) => *i = !*i,
                    Constant::U64(i) => *i = !*i,
                    Constant::Bool(i) => *i = !*i,
                    _ => self.context.context.report_error(node, format!("Cannot not expression of type {}", expr.value.get_type()))
                }
                _ => self.context.context.report_error(node, format!("Cannot not expression of type {}", expr.value.get_type()))
            }
        }
        Expression {
            node,
            value: expr.value,
        }
    }

    fn binop(
        &mut self,
        op: BinOp,
        lhs: Expression<'a>,
        rhs: Expression<'a>,
    ) -> Expression<'a> {
        let node = self.context.context.merge_nodes(lhs.node, rhs.node);
        let value = match op{
            BinOp::Add => {
                lhs.value
            }
            BinOp::Sub => match (lhs.value, rhs.value){
                (Value::Constant(l), Value::Constant(r)) => Value::Constant(match (l, r){
                    (Constant::I8(l), Constant::I8(r)) => Constant::I8(l.wrapping_sub(r)),
                    (Constant::I16(l), Constant::I16(r)) => Constant::I16(l.wrapping_sub(r)),
                    (Constant::I32(l), Constant::I32(r)) => Constant::I32(l.wrapping_sub(r)),
                    (Constant::I64(l), Constant::I64(r)) => Constant::I64(l.wrapping_sub(r)),
                    (Constant::U8(l), Constant::U8(r)) => Constant::U8(l.wrapping_sub(r)),
                    (Constant::U16(l), Constant::U16(r)) => Constant::U16(l.wrapping_sub(r)),
                    (Constant::U32(l), Constant::U32(r)) => Constant::U32(l.wrapping_sub(r)),
                    (Constant::U64(l), Constant::U64(r)) => Constant::U64(l.wrapping_sub(r)),
                    (Constant::F32(l), Constant::F32(r)) => Constant::F32(l-r),
                    (Constant::F64(l), Constant::F64(r)) => Constant::F64(l-r),
                    _ => { self.context.context.report_error(node, format!("Cannot subtract types {} and {}", lhs.value.get_type(), rhs.value.get_type())); l }
                }),
                _ => { self.context.context.report_error(node, format!("Cannot subtract types {} and {}", lhs.value.get_type(), rhs.value.get_type())); lhs.value }
            }
            BinOp::Mul => match (lhs.value, rhs.value){
                (Value::Constant(l), Value::Constant(r)) => Value::Constant(match (l, r){
                    (Constant::I8(l), Constant::I8(r)) => Constant::I8(l.wrapping_mul(r)),
                    (Constant::I16(l), Constant::I16(r)) => Constant::I16(l.wrapping_mul(r)),
                    (Constant::I32(l), Constant::I32(r)) => Constant::I32(l.wrapping_mul(r)),
                    (Constant::I64(l), Constant::I64(r)) => Constant::I64(l.wrapping_mul(r)),
                    (Constant::U8(l), Constant::U8(r)) => Constant::U8(l.wrapping_mul(r)),
                    (Constant::U16(l), Constant::U16(r)) => Constant::U16(l.wrapping_mul(r)),
                    (Constant::U32(l), Constant::U32(r)) => Constant::U32(l.wrapping_mul(r)),
                    (Constant::U64(l), Constant::U64(r)) => Constant::U64(l.wrapping_mul(r)),
                    (Constant::F32(l), Constant::F32(r)) => Constant::F32(l*r),
                    (Constant::F64(l), Constant::F64(r)) => Constant::F64(l*r),
                    _ => { self.context.context.report_error(node, format!("Cannot multiply types {} and {}", lhs.value.get_type(), rhs.value.get_type())); l }
                }),
                _ => { self.context.context.report_error(node, format!("Cannot multiply types {} and {}", lhs.value.get_type(), rhs.value.get_type())); lhs.value }
            }
            _ => {
                self.context.context.report_error(node, format!("Cannot binop types {} and {}", lhs.value.get_type(), rhs.value.get_type())); lhs.value
            }
            // BinOp::Div => {}
            // BinOp::Rem => {}
            // BinOp::And => {}
            // BinOp::Xor => {}
            // BinOp::Or => {}
            // BinOp::Shl => {}
            // BinOp::Shr => {}
            // BinOp::Lt => {}
            // BinOp::Lte => {}
            // BinOp::Gt => {}
            // BinOp::Gte => {}
            // BinOp::Eq => {}
            // BinOp::Ne => {}
        };
        Expression {
            node,
            value,
        }
    }
}
