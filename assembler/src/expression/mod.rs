pub mod args;
pub mod value;
pub use value::*;

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

use crate::assembler::instructions::Register;
use crate::context::{Context, Node, NodeId};
pub(crate) use crate::expression::{
    ArgumentsTypeHint, Constant, LabelMeta, LabelUse, Value, ValueType,
};
use crate::lex::{Number, Token, TypeHint};
use crate::util::IntoStrDelimable;
use std::fmt::{Display, Write};
use std::marker::PhantomData;
use std::ops::Index;
use crate::expression::args::CoercedArgs;

type Expression<'a> = Node<'a, Value<'a>>;

pub trait ExpressionEvaluatorContext<'a> {
    fn next(&mut self) -> Option<Node<'a, Token<'a>>>;
    fn peek(&mut self) -> Option<Node<'a, Token<'a>>>;
    fn context(&self) -> &Context<'a>;

    fn args(&mut self, fb: NodeId<'a>, hint: ArgumentsTypeHint) -> Node<'a, Vec<Expression<'a>>> {
        ExpressionEvaluator(self, PhantomData).parse_arguments(hint, fb)
    }

    fn coerced<T: CoercedArgs<'a>>(&mut self, fb: NodeId<'a>) -> T where Self: Sized {
        T::from(self, fb)
    }
}

pub struct ExpressionEvaluator<'a, 'b, T: ExpressionEvaluatorContext<'a> + ?Sized>(
    &'b mut T,
    PhantomData<&'a ()>,
);

impl<'a, 'b, T: ExpressionEvaluatorContext<'a> + ?Sized> ExpressionEvaluator<'a, 'b, T> {
    pub fn context(&self) -> &Context<'a> {
        self.0.context()
    }
    pub fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.0.next()
    }
    pub fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.0.peek()
    }

    fn parse_numeric_literal(
        &mut self,
        num: Number<'a>,
        n: NodeId<'a>,
        hint: ValueType,
    ) -> Constant<'a> {
        let (suffix, radix) = match num.get_hint() {
            TypeHint::Float if hint.is_integer() => (num.get_suffix().unwrap_or("f32"), 10),
            TypeHint::Float => (
                num.get_suffix()
                    .unwrap_or(hint.numeric_suffix().unwrap_or("f32")),
                10,
            ),
            TypeHint::Hex => (
                num.get_suffix().unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or("i32"),
                ),
                16,
            ),
            TypeHint::Bin => (
                num.get_suffix().unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or("i32"),
                ),
                2,
            ),
            TypeHint::Int => (
                num.get_suffix().unwrap_or(
                    hint.is_integer()
                        .then(|| hint.numeric_suffix())
                        .flatten()
                        .unwrap_or("i32"),
                ),
                10,
            ),
            TypeHint::Oct => (
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
                        self.context()
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
                        self.context()
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
                self.context()
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

    fn parse_expr_1(&mut self, hint: ValueType) -> Expression<'a> {
        let expr = match self.next() {
            Some(Node(Token::Ident(ident), node)) => match self.peek() {
                Some(Node(Token::LPar, _)) => {
                    let Node(args, args_node) = self.parse_arguments_delim(
                        ArgumentsTypeHint::None,
                        Token::LPar,
                        Token::RPar,
                    );
                    self.func(ident, node, args, args_node)
                }
                _ => self.handle_ident(ident, node, hint),
            },
            Some(Node(Token::TrueLiteral, node)) => {
                Node(Value::Constant(Constant::Bool(true)), node)
            }
            Some(Node(Token::FalseLiteral, node)) => {
                Node(Value::Constant(Constant::Bool(false)), node)
            }
            Some(Node(Token::StringLiteral(str), node)) => Node(
                Value::Constant(Constant::String(self.parse_string_literal(str, node))),
                node,
            ),
            Some(Node(Token::CharLiteral(str), node)) => Node(
                Value::Constant(Constant::Char(self.parse_char_literal(str, node))),
                node,
            ),
            Some(Node(Token::NumericLiteral(num), node)) => Node(
                Value::Constant(self.parse_numeric_literal(num, node, hint)),
                node,
            ),
            Some(Node(Token::LPar, lhs)) => {
                let mut arg = self.parse_expr(hint);
                match self.peek() {
                    Some(Node(Token::RPar, rhs)) => {
                        arg.1 = self.context().merge_nodes(lhs, rhs);
                        self.next();
                    }
                    t => _ = self.context().unexpected_token(t, Token::RPar, false),
                }
                arg
            }

            t => Node(
                hint.default_value(),
                self.context().unexpected_token(
                    t,
                    [
                        Token::Ident(""),
                        Token::LPar,
                        Token::CharLiteral(""),
                        Token::StringLiteral(""),
                        Token::FalseLiteral,
                        Token::TrueLiteral,
                        Token::NumericLiteral(Number::empty()),
                    ]
                    .iter()
                    .delim("|"),
                    false,
                ),
            ),
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
                    t => self.context().unexpected_token(t, Token::RBracket, false),
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

                    t => (
                        self.context().unexpected_token(t, Token::Ident(""), false),
                        "i32",
                    ),
                };
                return self.cast(expr, ty, node);
            }
            _ => {}
        }

        expr
    }

    fn parse_expr_2(&mut self, hint: ValueType, min_prec: u32) -> Expression<'a> {
        let mut lhs = self.parse_expr_1(hint);
        loop {
            let op = match self.peek() {
                Some(Node(Token::Plus, _)) if BinOp::Add.precedence() >= min_prec => BinOp::Add,
                Some(Node(Token::Minus, _)) if BinOp::Sub.precedence() >= min_prec => BinOp::Sub,

                Some(Node(Token::Star, _)) if BinOp::Mul.precedence() >= min_prec => BinOp::Mul,
                Some(Node(Token::Slash, _)) if BinOp::Div.precedence() >= min_prec => BinOp::Div,
                Some(Node(Token::Percent, _)) if BinOp::Rem.precedence() >= min_prec => BinOp::Rem,

                Some(Node(Token::LogicalOr, _)) if BinOp::Or.precedence() >= min_prec => BinOp::Or,
                Some(Node(Token::BitwiseXor, _)) if BinOp::Or.precedence() >= min_prec => BinOp::Or,

                Some(Node(Token::Ampersand, _)) if BinOp::And.precedence() >= min_prec => {
                    BinOp::And
                }
                Some(Node(Token::LogicalAnd, _)) if BinOp::And.precedence() >= min_prec => {
                    BinOp::And
                }

                Some(Node(Token::BitwiseXor, _)) if BinOp::Xor.precedence() >= min_prec => {
                    BinOp::Xor
                }

                Some(Node(Token::ShiftLeft, _)) if BinOp::Shl.precedence() >= min_prec => {
                    BinOp::Shl
                }
                Some(Node(Token::ShiftRight, _)) if BinOp::Shr.precedence() >= min_prec => {
                    BinOp::Shr
                }

                Some(Node(Token::GreaterThan, _)) if BinOp::Gt.precedence() >= min_prec => {
                    BinOp::Gt
                }
                Some(Node(Token::GreaterThanEq, _)) if BinOp::Gte.precedence() >= min_prec => {
                    BinOp::Gte
                }
                Some(Node(Token::LessThan, _)) if BinOp::Lt.precedence() >= min_prec => BinOp::Lt,
                Some(Node(Token::LessThanEq, _)) if BinOp::Lte.precedence() >= min_prec => {
                    BinOp::Lte
                }
                Some(Node(Token::Equals, _)) if BinOp::Eq.precedence() >= min_prec => BinOp::Eq,
                Some(Node(Token::NotEquals, _)) if BinOp::Ne.precedence() >= min_prec => BinOp::Ne,
                _ => break,
            };
            self.next();

            let rhs = self.parse_expr_2(hint, op.precedence() + 1);
            lhs = self.binop(op, lhs, rhs);
        }
        lhs
    }

    fn parse_expr_3(&mut self, hint: ValueType) -> Expression<'a> {
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

    fn parse_expr(&mut self, hint: ValueType) -> Expression<'a> {
        self.parse_expr_3(hint)
    }

    fn parse_char_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> char {
        let mut chars = repr.chars();
        if let Some(ok) = chars.next() {
            if chars.next().is_some() {
                self.context()
                    .report_error(n, "Char literal contains more than one char");
            }
            ok
        } else {
            self.context().report_error(n, "Char literal empty");
            '\0'
        }
    }

    fn parse_string_literal(&mut self, repr: &'a str, n: NodeId<'a>) -> &'a str {
        repr.into()
    }

    pub fn parse_arguments_delim(
        &mut self,
        hint: ArgumentsTypeHint<'_>,
        opening: Token<'a>,
        closing: Token<'a>,
    ) -> Node<'a, Vec<Expression<'a>>> {
        let mut args = Vec::new();

        let start = match self.peek() {
            Some(Node(t, n)) if t == opening => {
                self.next();
                n
            }
            t => self.context().unexpected_token(t, opening, true),
        };
        loop {
            args.push(self.parse_expr(hint[args.len()]));
            match self.peek() {
                Some(Node(Token::Comma, _)) => {
                    self.next();
                }
                Some(Node(t, end)) if t == closing => {
                    self.next();
                    return Node(args, self.context().merge_nodes(start, end));
                }
                t @ Some(_) => {
                    _ = self.context().unexpected_token(
                        t,
                        [Token::Comma, closing].iter().delim("|"),
                        true,
                    )
                }
                None => {
                    return Node(
                        args,
                        self.context().merge_nodes(
                            start,
                            self.context().unexpected_token(
                                None,
                                [Token::Comma, closing].iter().delim("|"),
                                true,
                            ),
                        ),
                    );
                }
            }
        }
    }

    pub fn parse_arguments(
        &mut self,
        hint: ArgumentsTypeHint<'_>,
        fallback_node: NodeId<'a>,
    ) -> Node<'a, Vec<Expression<'a>>> {
        let mut args = Vec::new();
        match self.peek() {
            Some(Node(Token::NewLine, _)) | None => return Node(args, fallback_node),
            _ => {}
        }
        loop {
            args.push(self.parse_expr(hint[args.len()]));
            match self.peek() {
                Some(Node(Token::Comma, _)) => {
                    self.next();
                }
                Some(Node(Token::NewLine, _)) | None => {
                    let left = args.first().map(|n| n.1).unwrap_or(fallback_node);
                    let right = args.last().map(|n| n.1).unwrap_or(fallback_node);
                    return Node(args, self.context().merge_nodes(left, right));
                }
                t => _ = self.context().unexpected_token(t, Token::Comma, false),
            }
        }
    }

    fn handle_ident(
        &mut self,
        ident: &'a str,
        node: NodeId<'a>,
        hint: ValueType,
    ) -> Expression<'a> {
        fn reg(node: NodeId, reg: u8) -> Expression {
            Node(Value::Register(Register(reg)), node)
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

            "f0" => reg(node, 32 + 0),
            "f1" => reg(node, 32 + 1),
            "f2" => reg(node, 32 + 2),
            "f3" => reg(node, 32 + 3),
            "f4" => reg(node, 32 + 4),
            "f5" => reg(node, 32 + 5),
            "f6" => reg(node, 32 + 6),
            "f7" => reg(node, 32 + 7),
            "f8" => reg(node, 32 + 8),
            "f9" => reg(node, 32 + 9),
            "f10" => reg(node, 32 + 10),
            "f11" => reg(node, 32 + 11),
            "f12" => reg(node, 32 + 12),
            "f13" => reg(node, 32 + 13),
            "f14" => reg(node, 32 + 14),
            "f15" => reg(node, 32 + 15),
            "f16" => reg(node, 32 + 16),
            "f17" => reg(node, 32 + 17),
            "f18" => reg(node, 32 + 18),
            "f19" => reg(node, 32 + 19),
            "f20" => reg(node, 32 + 20),
            "f21" => reg(node, 32 + 21),
            "f22" => reg(node, 32 + 22),
            "f23" => reg(node, 32 + 23),
            "f24" => reg(node, 32 + 24),
            "f25" => reg(node, 32 + 25),
            "f26" => reg(node, 32 + 26),
            "f27" => reg(node, 32 + 27),
            "f28" => reg(node, 32 + 28),
            "f29" => reg(node, 32 + 29),
            "f30" => reg(node, 32 + 30),
            "f31" => reg(node, 32 + 31),

            _ => Node(Value::Label(LabelUse::new(ident)), node),
        }
    }

    fn func(
        &mut self,
        func: &'a str,
        func_node: NodeId<'a>,
        args: Vec<Expression<'a>>,
        args_node: NodeId<'a>,
    ) -> Expression<'a> {
        let node = self.context().merge_nodes(func_node, args_node);
        let value = match func {
            "size" | "align" | "pcrel" | "absolute" => match &args[..] {
                [Node(Value::Label(l), node)] => {
                    if !matches!(l.meta, LabelMeta::Unset) {
                        self.context()
                            .report_error(node, "label metadata already set before")
                    }
                    Value::Label(LabelUse {
                        ident: l.ident,
                        offset: l.offset,
                        meta: match func {
                            "size" => LabelMeta::Size,
                            "align" => LabelMeta::Align,
                            "pcrel" => LabelMeta::PcRel,
                            "absolute" => LabelMeta::Absolute,
                            _ => LabelMeta::Unset,
                        },
                    })
                }
                unexpected => {
                    self.context().report_error(
                        args_node,
                        format!(
                            "Unexpected arguments found [{}] expected [{}]",
                            unexpected.iter().map(|e| e.0.get_type()).delim(", "),
                            ValueType::Label
                        ),
                    );
                    Value::Label(LabelUse {
                        ident: "",
                        offset: 0,
                        meta: LabelMeta::Unset,
                    })
                }
            },
            "align" => Value::Constant(Constant::I32(1)),
            "pcrel" => Value::Constant(Constant::I32(0)),
            "absolute" => Value::Constant(Constant::I32(0)),
            "format" => 'result: {
                let mut result = String::new();
                let len = args.len();
                let mut iter = args.into_iter();
                match iter.next() {
                    Some(Node(Value::Constant(Constant::String(format)), _)) => {
                        let expected = format.matches('%').count();
                        if expected != len - 1 {
                            self.context().report_error(
                                args_node,
                                format!(
                                    "wrong number of arguments provided expected {} found {}",
                                    expected,
                                    len - 1
                                ),
                            )
                        }
                        for part in format.split('%') {
                            result
                                .write_fmt(format_args!(
                                    "{}{}",
                                    part,
                                    iter.next()
                                        .map(|e| e.0)
                                        .unwrap_or(Value::Constant(Constant::String("")))
                                ))
                                .unwrap();
                        }
                        break 'result Value::Constant(Constant::String(
                            self.context().alloc_str(result),
                        ));
                    }
                    Some(Node(v, node)) => self.context().report_error(
                        node,
                        format!("expected string literal found {}", v.get_type()),
                    ),
                    None => self
                        .context()
                        .report_error(func_node, "expected string literal found nothing"),
                }

                Value::Constant(Constant::String(""))
            }
            _ => {
                self.context()
                    .report_error(func_node, format!("Unknown function {}", func));
                Value::Constant(Constant::I32(0))
            }
        };
        Node(value, node)
    }

    fn index(
        &mut self,
        lhs: Expression<'a>,
        rhs: Expression<'a>,
        closing: NodeId<'a>,
    ) -> Expression<'a> {
        let node = self.context().merge_nodes(lhs.1, closing);
        let value = match (rhs.0, lhs.0) {
            (Value::Register(r), Value::Constant(Constant::I32(i))) => Value::RegisterOffset(r, i),
            (Value::Constant(Constant::I32(i)), Value::Register(r)) => Value::RegisterOffset(r, i),
            (Value::RegisterOffset(r, o), Value::Constant(Constant::I32(i))) => {
                Value::RegisterOffset(r, o.wrapping_add(i))
            }
            (Value::Constant(Constant::I32(i)), Value::RegisterOffset(r, o)) => {
                Value::RegisterOffset(r, o.wrapping_add(i))
            }

            (Value::Label(l), Value::Constant(Constant::I32(i))) => Value::Label(LabelUse {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Constant(Constant::I32(i)), Value::Label(l)) => Value::Label(LabelUse {
                ident: l.ident,
                offset: l.offset.wrapping_add(i),
                meta: l.meta,
            }),
            (Value::Label(l), Value::Register(r)) => Value::LabelRegisterOffset(r, l),
            (Value::Register(r), Value::Label(l)) => Value::LabelRegisterOffset(r, l),

            (Value::Label(l), Value::RegisterOffset(r, i)) => Value::LabelRegisterOffset(
                r,
                LabelUse {
                    ident: l.ident,
                    offset: l.offset.wrapping_add(i),
                    meta: l.meta,
                },
            ),
            (Value::RegisterOffset(r, i), Value::Label(l)) => Value::LabelRegisterOffset(
                r,
                LabelUse {
                    ident: l.ident,
                    offset: l.offset.wrapping_add(i),
                    meta: l.meta,
                },
            ),
            _ => {
                self.context().report_error(
                    node,
                    format!(
                        "Cannot index {} with {}",
                        lhs.0.get_type(),
                        rhs.0.get_type()
                    ),
                );
                Value::RegisterOffset(Register(0), 0)
            }
        };
        Node(value, node)
    }

    fn cast_error(&mut self, expr: Expression<'a>, expected: ValueType) -> Value<'a> {
        self.context().report_error(
            expr.1,
            format!("Cannot cast {} to {}", expr.0.get_type(), expected),
        );
        expected.default_value()
    }

    fn cast(&mut self, expr: Expression<'a>, ty: &'a str, node_id: NodeId<'a>) -> Expression<'a> {
        macro_rules! integer {
            ($ident:ident, $ty:ty) => {
                match expr.0 {
                    Value::Constant(Constant::I8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::Bool(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::Char(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            };
        }

        macro_rules! float {
            ($ident:ident, $ty:ty) => {
                match expr.0 {
                    Value::Constant(Constant::I8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::I16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::I64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U8(i)) => Value::Constant(Constant::$ident(i as $ty)),
                    Value::Constant(Constant::U16(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::U64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F32(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    Value::Constant(Constant::F64(i)) => {
                        Value::Constant(Constant::$ident(i as $ty))
                    }
                    _ => self.cast_error(expr, ValueType::$ident),
                }
            };
        }

        let node = self.context().merge_nodes(expr.1, node_id);
        let value = match ty {
            "str" => Value::Constant(Constant::String(
                self.context().alloc_str(format!("{}", expr.0).as_str()),
            )),
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
            "char" => match expr.0 {
                Value::Constant(Constant::U8(i)) => Value::Constant(Constant::Char(i as char)),
                Value::Constant(Constant::U16(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::U32(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::U64(i)) => Value::Constant(Constant::Char(
                    char::from_u32(i as u32).unwrap_or(char::REPLACEMENT_CHARACTER),
                )),
                Value::Constant(Constant::Char(i)) => Value::Constant(Constant::Char(i)),
                _ => self.cast_error(expr, ValueType::Char),
            },
            _ => {
                self.context()
                    .report_error(node, format!("Unknown type {}", ty));
                expr.0
            }
        };
        Node(value, node)
    }

    fn unop(&mut self, op: UnOp, node: NodeId<'a>, mut expr: Expression<'a>) -> Expression<'a> {
        let node = self.context().merge_nodes(node, expr.1);
        match op {
            UnOp::Neg => match &mut expr.0 {
                Value::Constant(c) => match c {
                    Constant::I8(i) => *i = -*i,
                    Constant::I16(i) => *i = -*i,
                    Constant::I32(i) => *i = -*i,
                    Constant::I64(i) => *i = -*i,
                    Constant::F32(i) => *i = -*i,
                    Constant::F64(i) => *i = -*i,
                    _ => self.context().report_error(
                        node,
                        format!("Cannot negate expression of type {}", expr.0.get_type()),
                    ),
                },
                _ => self.context().report_error(
                    node,
                    format!("Cannot negate expression of type {}", expr.0.get_type()),
                ),
            },
            UnOp::Not => match &mut expr.0 {
                Value::Constant(c) => match c {
                    Constant::I8(i) => *i = !*i,
                    Constant::I16(i) => *i = !*i,
                    Constant::I32(i) => *i = !*i,
                    Constant::I64(i) => *i = !*i,
                    Constant::U8(i) => *i = !*i,
                    Constant::U16(i) => *i = !*i,
                    Constant::U32(i) => *i = !*i,
                    Constant::U64(i) => *i = !*i,
                    Constant::Bool(i) => *i = !*i,
                    _ => self.context().report_error(
                        node,
                        format!("Cannot not expression of type {}", expr.0.get_type()),
                    ),
                },
                _ => self.context().report_error(
                    node,
                    format!("Cannot not expression of type {}", expr.0.get_type()),
                ),
            },
        }
        Node(expr.0, node)
    }

    fn binop(&mut self, op: BinOp, lhs: Expression<'a>, rhs: Expression<'a>) -> Expression<'a> {
        macro_rules! constants_grouped {
            ($l:ident, $r:ident, $($integer:block)?, $($float:block)?, $($string:block)?, $($char:block)?, $($bool:block)?, $error:block) => {
                Value::Constant(match ($l, $r){
                    $(
                        (Constant::I8($l), Constant::I8($r)) => Constant::I8($integer),
                        (Constant::I16($l), Constant::I16($r)) => Constant::I16($integer),
                        (Constant::I32($l), Constant::I32($r)) => Constant::I32($integer),
                        (Constant::I64($l), Constant::I64($r)) => Constant::I64($integer),
                        (Constant::U8($l), Constant::U8($r)) => Constant::U8($integer),
                        (Constant::U16($l), Constant::U16($r)) => Constant::U16($integer),
                        (Constant::U32($l), Constant::U32($r)) => Constant::U32($integer),
                        (Constant::U64($l), Constant::U64($r)) => Constant::U64($integer),
                    )?
                    $(
                        (Constant::F32($l), Constant::F32($r)) => Constant::F32($float),
                        (Constant::F64($l), Constant::F64($r)) => Constant::F64($float),
                    )?
                    $(
                        (Constant::String($l), Constant::String($r)) => Constant::String($string),
                    )?
                    $(
                        (Constant::Char($l), Constant::Char($r)) => Constant::Char($char),
                    )?
                    $(
                        (Constant::Bool($l), Constant::Bool($r)) => Constant::Bool($bool),
                    )?

                    _ => $error
                })
            };
        }

        macro_rules! constants_bool {
            ($l:ident, $r:ident, $block:block, $error:block) => {
                Value::Constant(match ($l, $r) {
                    (Constant::I8($l), Constant::I8($r)) => Constant::Bool($block),
                    (Constant::I16($l), Constant::I16($r)) => Constant::Bool($block),
                    (Constant::I32($l), Constant::I32($r)) => Constant::Bool($block),
                    (Constant::I64($l), Constant::I64($r)) => Constant::Bool($block),
                    (Constant::U8($l), Constant::U8($r)) => Constant::Bool($block),
                    (Constant::U16($l), Constant::U16($r)) => Constant::Bool($block),
                    (Constant::U32($l), Constant::U32($r)) => Constant::Bool($block),
                    (Constant::U64($l), Constant::U64($r)) => Constant::Bool($block),
                    (Constant::F32($l), Constant::F32($r)) => Constant::Bool($block),
                    (Constant::F64($l), Constant::F64($r)) => Constant::Bool($block),
                    (Constant::String($l), Constant::String($r)) => Constant::Bool($block),
                    (Constant::Char($l), Constant::Char($r)) => Constant::Bool($block),
                    (Constant::Bool($l), Constant::Bool($r)) => Constant::Bool($block),
                    _ => $error,
                })
            };
        }

        let node = self.context().merge_nodes(lhs.1, rhs.1);
        let value = match op {
            BinOp::Add => match (lhs.0, rhs.0) {
                (Value::Constant(Constant::String(l)), r) => Value::Constant(Constant::String(
                    self.context().alloc_str(format!("{l}{r}")),
                )),
                (l, Value::Constant(Constant::String(r))) => Value::Constant(Constant::String(
                    self.context().alloc_str(format!("{l}{r}")),
                )),

                (Value::Register(r), Value::Constant(Constant::I32(i))) => {
                    Value::RegisterOffset(r, i)
                }
                (Value::Constant(Constant::I32(i)), Value::Register(r)) => {
                    Value::RegisterOffset(r, i)
                }
                (Value::RegisterOffset(r, o), Value::Constant(Constant::I32(i))) => {
                    Value::RegisterOffset(r, o.wrapping_add(i))
                }
                (Value::Constant(Constant::I32(i)), Value::RegisterOffset(r, o)) => {
                    Value::RegisterOffset(r, o.wrapping_add(i))
                }

                (Value::Label(l), Value::Constant(Constant::I32(i))) => Value::Label(LabelUse {
                    ident: l.ident,
                    offset: l.offset.wrapping_add(i),
                    meta: l.meta,
                }),
                (Value::Constant(Constant::I32(i)), Value::Label(l)) => Value::Label(LabelUse {
                    ident: l.ident,
                    offset: l.offset.wrapping_add(i),
                    meta: l.meta,
                }),

                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l.wrapping_add(r)},/*float*/{l+r},/*str*/,/*char*/,/*bool*/,
                    { self.context().report_error(node, format!("Cannot add types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot add types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Sub => match (lhs.0, rhs.0) {
                (Value::Register(r), Value::Constant(Constant::I32(i))) => {
                    Value::RegisterOffset(r, -i)
                }
                (Value::RegisterOffset(r, o), Value::Constant(Constant::I32(i))) => {
                    Value::RegisterOffset(r, o.wrapping_sub(i))
                }

                (Value::Label(l), Value::Constant(Constant::I32(i))) => Value::Label(LabelUse {
                    ident: l.ident,
                    offset: l.offset.wrapping_sub(i),
                    meta: l.meta,
                }),

                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l.wrapping_sub(r)},/*float*/{l-r},/*str*/,/*char*/,/*bool*/,
                    { self.context().report_error(node, format!("Cannot subtract types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot subtract types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Mul => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l.wrapping_mul(r)},/*float*/{l*r},/*str*/,/*char*/,/*bool*/,
                    { self.context().report_error(node, format!("Cannot multiply types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot multiply types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Div => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{
                        if r!=0{
                            l.wrapping_div(r)
                        }else{
                            self.context().report_error(node, "divide by zero");
                            0
                        }
                    },/*float*/{l/r},/*str*/,/*char*/,/*bool*/,
                    { self.context().report_error(node, format!("Cannot divide types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot divide types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Rem => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{
                        if r!=0{
                            l.wrapping_rem(r)
                        }else{
                            self.context().report_error(node, "remainder by zero");
                            0
                        }
                    },/*float*/{l%r},/*str*/,/*char*/,/*bool*/,
                    { self.context().report_error(node, format!("Cannot remainder types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot remainder types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Xor => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l^r},/*float*/,/*str*/,/*char*/,/*bool*/{l^r},
                    { self.context().report_error(node, format!("Cannot xor types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot xor types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::And => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l&r},/*float*/,/*str*/,/*char*/,/*bool*/{l&r},
                    { self.context().report_error(node, format!("Cannot and types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot and types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Or => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_grouped!(
                    l, r,/*int*/{l|r},/*float*/,/*str*/,/*char*/,/*bool*/{l|r},
                    { self.context().report_error(node, format!("Cannot or types {} and {}", lhs.0.get_type(), rhs.0.get_type())); l }
                ),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot or types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Shl => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => Value::Constant(match (l, r) {
                    (Constant::I8(l), Constant::U8(r)) => Constant::I8(l.wrapping_shl(r as u32)),
                    (Constant::I16(l), Constant::U8(r)) => Constant::I16(l.wrapping_shl(r as u32)),
                    (Constant::I32(l), Constant::U8(r)) => Constant::I32(l.wrapping_shl(r as u32)),
                    (Constant::I64(l), Constant::U8(r)) => Constant::I64(l.wrapping_shl(r as u32)),
                    (Constant::U8(l), Constant::U8(r)) => Constant::U8(l.wrapping_shl(r as u32)),
                    (Constant::U16(l), Constant::U8(r)) => Constant::U16(l.wrapping_shl(r as u32)),
                    (Constant::U32(l), Constant::U8(r)) => Constant::U32(l.wrapping_shl(r as u32)),
                    (Constant::U64(l), Constant::U8(r)) => Constant::U64(l.wrapping_shl(r as u32)),
                    (Constant::I8(l), Constant::U16(r)) => Constant::I8(l.wrapping_shl(r as u32)),
                    (Constant::I16(l), Constant::U16(r)) => Constant::I16(l.wrapping_shl(r as u32)),
                    (Constant::I32(l), Constant::U16(r)) => Constant::I32(l.wrapping_shl(r as u32)),
                    (Constant::I64(l), Constant::U16(r)) => Constant::I64(l.wrapping_shl(r as u32)),
                    (Constant::U8(l), Constant::U16(r)) => Constant::U8(l.wrapping_shl(r as u32)),
                    (Constant::U16(l), Constant::U16(r)) => Constant::U16(l.wrapping_shl(r as u32)),
                    (Constant::U32(l), Constant::U16(r)) => Constant::U32(l.wrapping_shl(r as u32)),
                    (Constant::U64(l), Constant::U16(r)) => Constant::U64(l.wrapping_shl(r as u32)),
                    (Constant::I8(l), Constant::U32(r)) => Constant::I8(l.wrapping_shl(r)),
                    (Constant::I16(l), Constant::U32(r)) => Constant::I16(l.wrapping_shl(r)),
                    (Constant::I32(l), Constant::U32(r)) => Constant::I32(l.wrapping_shl(r)),
                    (Constant::I64(l), Constant::U32(r)) => Constant::I64(l.wrapping_shl(r)),
                    (Constant::U8(l), Constant::U32(r)) => Constant::U8(l.wrapping_shl(r)),
                    (Constant::U16(l), Constant::U32(r)) => Constant::U16(l.wrapping_shl(r)),
                    (Constant::U32(l), Constant::U32(r)) => Constant::U32(l.wrapping_shl(r)),
                    (Constant::U64(l), Constant::U32(r)) => Constant::U64(l.wrapping_shl(r)),
                    _ => {
                        self.context().report_error(
                            node,
                            format!(
                                "Cannot shift left types {} and {}",
                                lhs.0.get_type(),
                                rhs.0.get_type()
                            ),
                        );
                        l
                    }
                }),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot shift left types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Shr => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => Value::Constant(match (l, r) {
                    (Constant::I8(l), Constant::U8(r)) => Constant::I8(l.wrapping_shr(r as u32)),
                    (Constant::I16(l), Constant::U8(r)) => Constant::I16(l.wrapping_shr(r as u32)),
                    (Constant::I32(l), Constant::U8(r)) => Constant::I32(l.wrapping_shr(r as u32)),
                    (Constant::I64(l), Constant::U8(r)) => Constant::I64(l.wrapping_shr(r as u32)),
                    (Constant::U8(l), Constant::U8(r)) => Constant::U8(l.wrapping_shr(r as u32)),
                    (Constant::U16(l), Constant::U8(r)) => Constant::U16(l.wrapping_shr(r as u32)),
                    (Constant::U32(l), Constant::U8(r)) => Constant::U32(l.wrapping_shr(r as u32)),
                    (Constant::U64(l), Constant::U8(r)) => Constant::U64(l.wrapping_shr(r as u32)),
                    (Constant::I8(l), Constant::U16(r)) => Constant::I8(l.wrapping_shr(r as u32)),
                    (Constant::I16(l), Constant::U16(r)) => Constant::I16(l.wrapping_shr(r as u32)),
                    (Constant::I32(l), Constant::U16(r)) => Constant::I32(l.wrapping_shr(r as u32)),
                    (Constant::I64(l), Constant::U16(r)) => Constant::I64(l.wrapping_shr(r as u32)),
                    (Constant::U8(l), Constant::U16(r)) => Constant::U8(l.wrapping_shr(r as u32)),
                    (Constant::U16(l), Constant::U16(r)) => Constant::U16(l.wrapping_shr(r as u32)),
                    (Constant::U32(l), Constant::U16(r)) => Constant::U32(l.wrapping_shr(r as u32)),
                    (Constant::U64(l), Constant::U16(r)) => Constant::U64(l.wrapping_shr(r as u32)),
                    (Constant::I8(l), Constant::U32(r)) => Constant::I8(l.wrapping_shr(r)),
                    (Constant::I16(l), Constant::U32(r)) => Constant::I16(l.wrapping_shr(r)),
                    (Constant::I32(l), Constant::U32(r)) => Constant::I32(l.wrapping_shr(r)),
                    (Constant::I64(l), Constant::U32(r)) => Constant::I64(l.wrapping_shr(r)),
                    (Constant::U8(l), Constant::U32(r)) => Constant::U8(l.wrapping_shr(r)),
                    (Constant::U16(l), Constant::U32(r)) => Constant::U16(l.wrapping_shr(r)),
                    (Constant::U32(l), Constant::U32(r)) => Constant::U32(l.wrapping_shr(r)),
                    (Constant::U64(l), Constant::U32(r)) => Constant::U64(l.wrapping_shr(r)),
                    _ => {
                        self.context().report_error(
                            node,
                            format!(
                                "Cannot shift right types {} and {}",
                                lhs.0.get_type(),
                                rhs.0.get_type()
                            ),
                        );
                        l
                    }
                }),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot shift right types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },

            BinOp::Lt => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_bool!(l, r, { l < r }, {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    l
                }),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Lte => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_bool!(l, r, { l <= r }, {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    l
                }),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Gt => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_bool!(l, r, { l > r }, {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    l
                }),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Gte => match (lhs.0, rhs.0) {
                (Value::Constant(l), Value::Constant(r)) => constants_bool!(l, r, { l >= r }, {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    l
                }),
                _ => {
                    self.context().report_error(
                        node,
                        format!(
                            "Cannot compare types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    lhs.0
                }
            },
            BinOp::Eq => {
                if lhs.0.get_type() == rhs.0.get_type() {
                    Value::Constant(Constant::Bool(lhs.0 == rhs.0))
                } else {
                    self.context().report_error(
                        node,
                        format!(
                            "cannot compare differing types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    Value::Constant(Constant::Bool(lhs.0 == rhs.0))
                }
            }
            BinOp::Ne => {
                if lhs.0.get_type() == rhs.0.get_type() {
                    Value::Constant(Constant::Bool(lhs.0 != rhs.0))
                } else {
                    self.context().report_error(
                        node,
                        format!(
                            "cannot compare differing types {} and {}",
                            lhs.0.get_type(),
                            rhs.0.get_type()
                        ),
                    );
                    Value::Constant(Constant::Bool(lhs.0 != rhs.0))
                }
            }
        };
        Node(value, node)
    }
}
