use crate::assembler::Assembler;
use crate::assembler::instructions::Register;
use crate::context::{Node, NodeId};
use crate::lex::{Number, Token};
use std::ops::Index;

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ExpressionType {
    Unknown,

    String,
    CString,

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

impl ExpressionType {
    pub fn default_value<'a>(&self, node: NodeId<'a>) -> Expression<'a> {
        let ty = *self;
        match self {
            ExpressionType::Unknown => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::I32(0)),
            },
            ExpressionType::String => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::String("")),
            },
            ExpressionType::CString => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::String("\0")),
            },
            ExpressionType::Indexed => todo!(),
            ExpressionType::Register => Expression {
                ty,
                node,
                kind: ExpressionKind::Register(Register(0)),
            },
            ExpressionType::Label => Expression {
                ty,
                node,
                kind: ExpressionKind::Ident(""),
            },
            ExpressionType::I8 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::I8(0)),
            },
            ExpressionType::I16 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::I16(0)),
            },
            ExpressionType::I32 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::I32(0)),
            },
            ExpressionType::I64 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::I16(0)),
            },
            ExpressionType::U8 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::U8(0)),
            },
            ExpressionType::U16 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::U16(0)),
            },
            ExpressionType::U32 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::U32(0)),
            },
            ExpressionType::U64 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::U64(0)),
            },
            ExpressionType::F32 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::F32(0.0)),
            },
            ExpressionType::F64 => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::F64(0.0)),
            },
            ExpressionType::Bool => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::Bool(false)),
            },
            ExpressionType::Char => Expression {
                ty,
                node,
                kind: ExpressionKind::Constant(Constant::Char('\0')),
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Expression<'a> {
    pub ty: ExpressionType,
    pub node: NodeId<'a>,
    pub kind: ExpressionKind<'a>,
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
    Sub,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum ExpressionKind<'a> {
    Constant(Constant<'a>),
    Ident(&'a str),
    Register(Register),
    BinOp(&'a Expression<'a>, BinOp, &'a Expression<'a>),
    UnOp(UnOp, &'a Expression<'a>),
    Func(&'a str, &'a [Expression<'a>]),
    Cast(&'a str, ExpressionType, &'a Expression<'a>),
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
            Constant::Char(char) => ConvertResult::Success(char as u32),
            Constant::Bool(bool) => ConvertResult::Success(bool as u32),
        }
    }
}

impl ExpressionType {
    pub fn size(&self) -> Option<u32> {
        match self {
            ExpressionType::Unknown => None,
            ExpressionType::Indexed => None,
            ExpressionType::Register => None,
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

    pub fn align(&self) -> u32 {
        match self {
            ExpressionType::Unknown => 0,
            ExpressionType::Indexed => 0,
            ExpressionType::Register => 0,
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

    pub fn numeric_suffix(&self) -> Option<&'static str> {
        match self {
            ExpressionType::Unknown => None,
            ExpressionType::Register => None,
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

    pub fn is_integer(&self) -> bool {
        match self {
            ExpressionType::Unknown => false,
            ExpressionType::Register => false,
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
            ArgumentsTypeHint::Individual(ty) => ty.get(index).unwrap_or(&ExpressionType::Unknown),
            ArgumentsTypeHint::None => &ExpressionType::Unknown,
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
                    if let ExpressionKind::Constant(c) = hint.default_value(n).kind {
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
                ty: ExpressionType::Unknown,
                kind: ExpressionKind::Constant(Constant::Bool(true)),
                node,
            },
            Some(Node(Token::FalseLiteral, node)) => Expression {
                ty: ExpressionType::Unknown,
                kind: ExpressionKind::Constant(Constant::Bool(false)),
                node,
            },
            Some(Node(Token::StringLiteral(str), node)) => Expression {
                ty: ExpressionType::Unknown,
                kind: ExpressionKind::Constant(Constant::String(
                    self.parse_string_literal(str, node),
                )),
                node,
            },
            Some(Node(Token::CharLiteral(str), node)) => Expression {
                ty: ExpressionType::Unknown,
                kind: ExpressionKind::Constant(Constant::Char(self.parse_char_literal(str, node))),
                node,
            },
            Some(Node(Token::NumericLiteral(num), node)) => Expression {
                ty: ExpressionType::Unknown,
                kind: ExpressionKind::Constant(self.parse_numeric_literal(num, node, hint)),
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

            Some(Node(t, n)) => {
                self.context
                    .context
                    .report_error(n, format!("Unexpected token '{t:?}'"));
                hint.default_value(n)
            }
            None => {
                self.context
                    .context
                    .report_error_eof("Expected token but found eof");
                hint.default_value(self.context.context.top_src_eof())
            }
        };

        match self.peek() {
            Some(Node(Token::LBracket, node)) => {
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
            Some(Node(Token::Ident("as"), node)) => {
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

    fn parse_expr_2(&mut self, hint: ExpressionType) -> Expression<'a> {
        self.parse_expr_1(hint)
    }

    fn parse_expr_3(&mut self, hint: ExpressionType) -> Expression<'a> {
        match self.peek() {
            Some(Node(Token::Minus, node)) => {
                self.next();
                let expr = self.parse_expr_3(hint);
                self.unop(UnOp::Sub, node, expr)
            }
            Some(Node(Token::LogicalNot, node)) => {
                self.next();
                let expr = self.parse_expr_3(hint);
                self.unop(UnOp::Not, node, expr)
            }
            _ => self.parse_expr_2(hint),
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
                kind: ExpressionKind::Register(Register(reg)),
                ty: ExpressionType::Register,
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
                self.context
                    .context
                    .report_error(node, format!("Unexpected identifier '{ident}'"));
                hint.default_value(node)
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
        Expression {
            ty: ExpressionType::Unknown,
            node: self.context.context.merge_nodes(func_node, args_node),
            kind: ExpressionKind::Func(func, self.context.context.alloc_slice(&args[..])),
        }
    }

    fn index(
        &mut self,
        lhs: Expression<'a>,
        rhs: Expression<'a>,
        closing: NodeId<'a>,
    ) -> Expression<'a> {
        Expression {
            ty: ExpressionType::Indexed,
            node: self.context.context.merge_nodes(lhs.node, closing),
            kind: ExpressionKind::Index(
                self.context.context.alloc(lhs),
                self.context.context.alloc(rhs),
            ),
        }
    }

    fn cast(&mut self, expr: Expression<'a>, ty: &'a str, node_id: NodeId<'a>) -> Expression<'a> {
        Expression {
            ty: ExpressionType::Unknown,
            node: self.context.context.merge_nodes(expr.node, node_id),
            kind: ExpressionKind::Cast(
                ty,
                ExpressionType::Unknown,
                self.context.context.alloc(expr),
            ),
        }
    }

    fn unop(&mut self, op: UnOp, node: NodeId<'a>, expr: Expression<'a>) -> Expression<'a> {
        Expression {
            ty: expr.ty,
            node: self.context.context.merge_nodes(node, expr.node),
            kind: ExpressionKind::UnOp(op, self.context.context.alloc(expr)),
        }
    }

    fn binop(
        &mut self,
        op: BinOp,
        node: NodeId<'a>,
        lhs: Expression<'a>,
        rhs: Expression<'a>,
    ) -> Expression<'a> {
        Expression {
            ty: ExpressionType::Unknown,
            node: self.context.context.merge_nodes(lhs.node, rhs.node),
            kind: ExpressionKind::BinOp(
                self.context.context.alloc(lhs),
                op,
                self.context.context.alloc(rhs),
            ),
        }
    }
}
