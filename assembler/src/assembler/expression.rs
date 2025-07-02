use std::ops::Index;
use crate::assembler::Assembler;
use crate::context::{Node, NodeId};
use crate::lex::{Number, Token};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ExpressionType{
    Unknown,

    String,
    CString,

    Indexed,

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

impl ExpressionType{
    pub fn default_value<'a>(&self, node: NodeId<'a>) -> Expression<'a>{
        let ty = *self;
        match self{
            ExpressionType::Unknown => Expression{ ty, node, kind: Kind::Constant(Constant::I32(0)), },
            ExpressionType::String => Expression{ ty, node, kind: Kind::Constant(Constant::String("")), },
            ExpressionType::CString => Expression{ ty, node, kind: Kind::Constant(Constant::String("\0")), },
            ExpressionType::Indexed => todo!(),
            ExpressionType::Label => Expression{ ty, node, kind: Kind::Ident(""), },
            ExpressionType::I8 => Expression{ ty, node, kind: Kind::Constant(Constant::I8(0)), },
            ExpressionType::I16 => Expression{ ty, node, kind: Kind::Constant(Constant::I16(0)), },
            ExpressionType::I32 => Expression{ ty, node, kind: Kind::Constant(Constant::I32(0)), },
            ExpressionType::I64 => Expression{ ty, node, kind: Kind::Constant(Constant::I16(0)), },
            ExpressionType::U8 => Expression{ ty, node, kind: Kind::Constant(Constant::U8(0)), },
            ExpressionType::U16 => Expression{ ty, node, kind: Kind::Constant(Constant::U16(0)), },
            ExpressionType::U32 => Expression{ ty, node, kind: Kind::Constant(Constant::U32(0)), },
            ExpressionType::U64 => Expression{ ty, node, kind: Kind::Constant(Constant::U64(0)), },
            ExpressionType::F32 => Expression{ ty, node, kind: Kind::Constant(Constant::F32(0.0)), },
            ExpressionType::F64 => Expression{ ty, node, kind: Kind::Constant(Constant::F64(0.0)), },
            ExpressionType::Bool => Expression{ ty, node, kind: Kind::Constant(Constant::Bool(false)), },
            ExpressionType::Char => Expression{ ty, node, kind: Kind::Constant(Constant::Char('\0')), },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Expression<'a> {
    pub ty: ExpressionType,
    pub node: NodeId<'a>,
    pub kind: Kind<'a>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum BinOp{
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    And,
    Xor,
    Or,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum UnOp{
    Sub,
    Not
}

#[derive(Debug, Clone, Copy)]
pub enum Kind<'a>{
    Constant(Constant<'a>),
    Ident(&'a str),
    BinOp(&'a Expression<'a>, BinOp, &'a Expression<'a>),
    UnOp(BinOp, &'a Expression<'a>),
    Func(&'a str, &'a [Expression<'a>]),
    Cast(&'a str, ExpressionType),
    Index(&'a Expression<'a>, &'a Expression<'a>),
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
            Constant::Char(_) => ConvertResult::Failure,
            Constant::Bool(_) => ConvertResult::Failure,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParsedArgument<'a> {
    Label(&'a str, i64),
    Register(u8),
    Constant(Constant<'a>),
}

impl ExpressionType{
    pub fn size(&self) -> Option<u32>{
        match self{
            ExpressionType::Unknown => None,
            ExpressionType::Indexed => None,
            ExpressionType::String => None,
            ExpressionType::CString => None,
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

    pub fn align(&self) -> u32{
        match self{
            ExpressionType::Unknown => 0,
            ExpressionType::Indexed => 0,
            ExpressionType::String => 1,
            ExpressionType::CString => 1,
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

    pub fn numeric_suffix(&self) -> Option<&'static str>{
        match self{
            ExpressionType::Unknown => None,
            ExpressionType::Indexed => None,
            ExpressionType::String => None,
            ExpressionType::CString => None,
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

    pub fn is_integer(&self) -> bool{
        match self{
            ExpressionType::Unknown => false,
            ExpressionType::Indexed => false,
            ExpressionType::String => false,
            ExpressionType::CString => false,
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
pub enum ArgumentsTypeHint<'a>{
    Mono(ExpressionType),
    Individual(&'a [ExpressionType]),
    None
}

impl<'a> Index<usize> for ArgumentsTypeHint<'a>{
    type Output = ExpressionType;

    fn index(&self, index: usize) -> &Self::Output {
        match self{
            ArgumentsTypeHint::Mono(ty) => ty,
            ArgumentsTypeHint::Individual(ty) => ty.get(index).unwrap_or(&ExpressionType::Unknown),
            ArgumentsTypeHint::None => &ExpressionType::Unknown
        }
    }
}

impl<'a> Assembler<'a> {
    fn parse_numeric_literal(&mut self, num: Number<'a>, n: NodeId<'a>, hint: ExpressionType) -> Constant<'a> {
        let (suffix, radix) = match num.get_hint() {
            crate::lex::TypeHint::Float if hint.is_integer() => (num.get_suffix().unwrap_or("f32"), 10),
            crate::lex::TypeHint::Float => (num.get_suffix().unwrap_or(hint.numeric_suffix().unwrap_or("f32")), 10),
            crate::lex::TypeHint::Hex => (num.get_suffix().unwrap_or(hint.is_integer().then(||hint.numeric_suffix()).flatten().unwrap_or("i32")), 16),
            crate::lex::TypeHint::Bin => (num.get_suffix().unwrap_or(hint.is_integer().then(||hint.numeric_suffix()).flatten().unwrap_or("i32")), 2),
            crate::lex::TypeHint::Int => (num.get_suffix().unwrap_or(hint.is_integer().then(||hint.numeric_suffix()).flatten().unwrap_or("i32")), 10),
            crate::lex::TypeHint::Oct => (num.get_suffix().unwrap_or(hint.is_integer().then(||hint.numeric_suffix()).flatten().unwrap_or("i32")), 8),
        };

        macro_rules! integer {
            ($num:ty) => {
                <$num>::from_str_radix(num.get_num(), radix)
                    .inspect_err(|e| {
                        self.context
                            .context
                            .borrow_mut()
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
                            .borrow_mut()
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
                    .borrow_mut()
                    .report_error(n, format!("Unknown numeric suffix '{suffix}'"));
                Constant::I32(0)
            }
        }
    }
    
    fn parse_1(&mut self, hint: ExpressionType) -> Expression<'a>{
        
        let expr= match self.next(){
            Some(Node(Token::Ident(ident), node)) => {
                self.context.context.borrow_mut().report_error(node, format!("Unexpected identifier '{ident}'"));
                hint.default_value(node)
            }
            Some(Node(Token::TrueLiteral, node)) => Expression{
                ty: ExpressionType::Unknown,
                kind: Kind::Constant(Constant::Bool(true)),
                node,
            },
            Some(Node(Token::FalseLiteral, node)) => Expression{
                ty: ExpressionType::Unknown,
                kind: Kind::Constant(Constant::Bool(false)),
                node,
            },
            Some(Node(Token::StringLiteral(str), node)) => Expression{
                ty: ExpressionType::Unknown,
                kind: Kind::Constant(Constant::String(self.parse_string_literal(str, node))),
                node,
            },
            Some(Node(Token::CharLiteral(str), node)) => Expression{
                ty: ExpressionType::Unknown,
                kind: Kind::Constant(Constant::Char(self.parse_char_literal(str, node))),
                node,
            },
            Some(Node(Token::NumericLiteral(num), node)) => Expression{
                ty: ExpressionType::Unknown,
                kind: Kind::Constant(self.parse_numeric_literal(num, node, hint)),
                node,
            },
            Some(Node(Token::LPar, lhs)) => {
                let mut arg = self.parse_arg(hint);
                match self.peek(){
                    Some(Node(Token::RPar, rhs)) => {
                        arg.node = self.context.context.borrow_mut().merge_nodes(lhs, rhs);
                        self.next();
                    }
                    Some(Node(t, rhs)) => {
                        self.context.context.borrow_mut().report_error(rhs, format!("Unexpected token '{t:?}'"));
                        arg.node = self.context.context.borrow_mut().merge_nodes(lhs, rhs);
                    }
                    None => {
                        self.context.context.borrow_mut().report_error_eof("Expected rpar but found eof");
                    }
                }
                arg
            }
            
            Some(Node(t, n)) => {
                self.context.context.borrow_mut().report_error(n, format!("Unexpected token '{t:?}'"));
                hint.default_value(n)
            }
            None => {
                self.context.context.borrow_mut().report_error_eof("Expected token but found eof");
                hint.default_value(self.context.context.borrow_mut().top_src_eof())
            }
        };
        
        match self.peek(){
            Some(Node(Token::LBracket, node)) => {
                self.next();
            }
            _ => {}
        }
        
        
        expr
    }
    
    fn parse_arg(&mut self, hint: ExpressionType) -> Expression<'a>{
        todo!()
    }

    fn parse_argument(&mut self, hint: ExpressionType) -> Option<Node<'a, ParsedArgument<'a>>> {
        match self.peek() {
            Some(Node(Token::NumericLiteral(num), n)) => {
                self.next();
                todo!()
                
                // Some(
                //     self.parse_numeric_literal(num, n, hint)
                //         .map(ParsedArgument::Constant),
                // )
            }
            Some(Node(Token::Ident(str), n)) => {
                self.next();
                Some(Node(ParsedArgument::Label(str, 0), n))
            }
            Some(Node(Token::StringLiteral(str), n)) => {
                self.next();
                Some(Node(
                    ParsedArgument::Constant(Constant::String(
                        self.parse_string_literal(str, n),
                    )),
                    n,
                ))
            }
            Some(Node(Token::CharLiteral(str), n)) => {
                self.next();
                Some(Node(
                    ParsedArgument::Constant(Constant::Char(
                        self.parse_char_literal(str, n),
                    )),
                    n,
                ))
            }
            Some(Node(Token::FalseLiteral, n)) => {
                self.next();
                Some(Node(ParsedArgument::Constant(Constant::Bool(false)), n))
            }
            Some(Node(Token::TrueLiteral, n)) => {
                self.next();
                Some(Node(ParsedArgument::Constant(Constant::Bool(true)), n))
            }
            _ => None,
        }
    }

    fn parse_char_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> char {
        let mut chars = repr.chars();
        if let Some(ok) = chars.next() {
            if chars.next().is_some() {
                self.context
                    .context
                    .borrow_mut()
                    .report_error(n, "Char literal contains more than one char");
            }
            ok
        } else {
            self.context
                .context
                .borrow_mut()
                .report_error(n, "Char literal empty");
            '\0'
        }
    }

    fn parse_string_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> &'a str {
        repr.into()
    }

    pub(super) fn gather_arguments(
        &mut self,
        mnemonic: &'a str,
        n: NodeId<'a>,
        hint: ArgumentsTypeHint<'_>,
    ) -> Result<Vec<Node<'a, ParsedArgument<'a>>>, ()> {
        let mut args = Vec::new();
        while !matches!(self.peek(), Some(Node(Token::NewLine, _)) | None) {
            args.push(self.parse_argument(hint[args.len()]).ok_or(())?);
            match self.peek() {
                Some(Node(Token::Comma, _)) => {
                    self.next();
                }
                Some(Node(Token::NewLine, _)) | None => {}
                Some(Node(t, n)) => {
                    self.context
                        .context
                        .borrow_mut()
                        .report_error(n, format!("Expected comma found '{t:?}'"));
                }
            }
        }
        Ok(args)
    }
}
