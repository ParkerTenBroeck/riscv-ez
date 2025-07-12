pub mod args;
pub mod value;
pub use value::*;
pub mod base;
pub use base::*;

use crate::assembler::AssemblyLanguage;
use crate::context::{Context, Node, NodeId};
use crate::expression::args::CoercedArgs;
use crate::lex::{Number, Token};
use crate::util::IntoStrDelimable;
use std::marker::PhantomData;

pub type NodeVal<'a, T> = Node<'a, Value<'a, T>>;

pub trait ExpressionEvaluatorContext<'a, L: AssemblyLanguage<'a>>: Sized {
    fn next(&mut self) -> Option<Node<'a, Token<'a>>>;
    fn peek(&mut self) -> Option<Node<'a, Token<'a>>>;
    fn context(&mut self) -> &mut Context<'a>;
    fn parse_ident(
        &mut self,
        ident: &'a str,
        node: NodeId<'a>,
        hint: ValueType<'a, L>,
    ) -> NodeVal<'a, L>;

    fn expr(&mut self, hint: ValueType<'a, L>) -> Node<'a, Value<'a, L>> {
        ExpressionEvaluator(self, PhantomData).parse_expr(hint)
    }
    fn args(
        &mut self,
        fb: NodeId<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        ExpressionEvaluator(self, PhantomData).parse_arguments(hint, fb)
    }

    fn args_delim(
        &mut self,
        start: Token<'a>,
        end: Token<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        ExpressionEvaluator(self, PhantomData).parse_arguments_delim(hint, start, end)
    }

    fn coerced<T: CoercedArgs<'a, L>>(&mut self, fb: NodeId<'a>) -> T
    where
        Self: Sized,
    {
        T::args(self, fb)
    }

    fn coerced_delim<T: CoercedArgs<'a, L>>(&mut self, start: Token<'a>, end: Token<'a>) -> T
    where
        Self: Sized,
    {
        T::args_delim(self, start, end)
    }
}

pub struct ExpressionEvaluator<
    'a,
    'b,
    L: AssemblyLanguage<'a>,
    T: ExpressionEvaluatorContext<'a, L> + ?Sized,
>(&'b mut T, PhantomData<(&'a (), L)>);

impl<'a, 'b, L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>
    ExpressionEvaluator<'a, 'b, L, T>
{
    pub fn context(&mut self) -> &mut Context<'a> {
        self.0.context()
    }
    pub fn next(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.0.next()
    }
    pub fn peek(&mut self) -> Option<Node<'a, Token<'a>>> {
        self.0.peek()
    }

    fn parse_expr_1(&mut self, hint: ValueType<'a, L>, negated: bool) -> NodeVal<'a, L> {
        let expr = match self.next() {
            Some(Node(Token::Ident(ident), node)) => match self.peek() {
                Some(Node(Token::LPar, _)) => {
                    let Node(args, args_node) = self.parse_arguments_delim(
                        ArgumentsTypeHint::None,
                        Token::LPar,
                        Token::RPar,
                    );
                    self.func_base(ident, node, args, args_node)
                }
                _ => self.0.parse_ident(ident, node, hint),
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
                Value::Constant(self.parse_numeric_literal_base(num, node, hint, negated)),
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
                return self.index_base(expr, rhs, node);
            }
            _ => {}
        }

        expr
    }

    fn parse_expr_2(&mut self, hint: ValueType<'a, L>, negated: bool) -> NodeVal<'a, L> {
        match self.peek() {
            Some(Node(Token::Minus, node)) => {
                self.next();
                let expr = self.parse_expr_2(hint, !negated);
                self.unop_base(unop::UnOp::Neg, node, expr)
            }
            Some(Node(Token::LogicalNot, node)) => {
                self.next();
                let expr = self.parse_expr_2(hint, false);
                self.unop_base(unop::UnOp::Not, node, expr)
            }
            _ => self.parse_expr_1(hint, negated),
        }
    }

    fn parse_expr_3(&mut self, hint: ValueType<'a, L>,) -> NodeVal<'a, L>{
        let expr = self.parse_expr_2(hint, false);
        match self.peek() {
            Some(Node(Token::Ident("as"), _)) => {
                self.next();

                let (node, ty) = match self.peek() {
                    Some(Node(Token::Ident(ty), node)) => {
                        self.next();
                        (node, ty)
                    }

                    t => {
                        self.context().unexpected_token(t, Token::Ident(""), false);
                        return expr;
                    }
                };
                self.cast_base(expr, ty, node)
            }
            _ => expr,
        }
    }

    fn parse_expr_4(&mut self, hint: ValueType<'a, L>, min_prec: u32) -> NodeVal<'a, L> {
        use Token as T;
        use binop::BinOp as BO;
        let mut lhs = self.parse_expr_3(hint);
        loop {
            let op = match self.peek() {
                Some(Node(T::Plus, _)) if BO::Add.precedence() >= min_prec => BO::Add,
                Some(Node(T::Minus, _)) if BO::Sub.precedence() >= min_prec => BO::Sub,
                Some(Node(T::Star, _)) if BO::Mul.precedence() >= min_prec => BO::Mul,
                Some(Node(T::Slash, _)) if BO::Div.precedence() >= min_prec => BO::Div,
                Some(Node(T::Percent, _)) if BO::Rem.precedence() >= min_prec => BO::Rem,
                Some(Node(T::LogicalOr, _)) if BO::Or.precedence() >= min_prec => BO::Or,
                Some(Node(T::BitwiseOr, _)) if BO::Or.precedence() >= min_prec => BO::Or,
                Some(Node(T::Ampersand, _)) if BO::And.precedence() >= min_prec => BO::And,
                Some(Node(T::LogicalAnd, _)) if BO::And.precedence() >= min_prec => BO::And,
                Some(Node(T::BitwiseXor, _)) if BO::Xor.precedence() >= min_prec => BO::Xor,
                Some(Node(T::ShiftLeft, _)) if BO::Shl.precedence() >= min_prec => BO::Shl,
                Some(Node(T::ShiftRight, _)) if BO::Shr.precedence() >= min_prec => BO::Shr,
                Some(Node(T::GreaterThan, _)) if BO::Gt.precedence() >= min_prec => BO::Gt,
                Some(Node(T::GreaterThanEq, _)) if BO::Gte.precedence() >= min_prec => BO::Gte,
                Some(Node(T::LessThan, _)) if BO::Lt.precedence() >= min_prec => BO::Lt,
                Some(Node(T::LessThanEq, _)) if BO::Lte.precedence() >= min_prec => BO::Lte,
                Some(Node(T::Equals, _)) if BO::Eq.precedence() >= min_prec => BO::Eq,
                Some(Node(T::NotEquals, _)) if BO::Ne.precedence() >= min_prec => BO::Ne,
                _ => break,
            };
            self.next();

            let shift = matches!(op, BO::Shl | BO::Shr);

            let rhs = self.parse_expr_4(
                if shift { ValueType::U32 } else { hint },
                op.precedence() + 1,
            );
            lhs = self.binop_base(op, lhs, rhs);
        }
        lhs
    }

    pub fn parse_expr(&mut self, hint: ValueType<'a, L>) -> NodeVal<'a, L> {
        self.parse_expr_4(hint, 0)
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

    fn parse_string_literal(&mut self, repr: &'a str, _: NodeId<'a>) -> &'a str {
        repr
    }

    pub fn parse_arguments_delim(
        &mut self,
        hint: ArgumentsTypeHint<'a, '_, L>,
        opening: Token<'a>,
        closing: Token<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
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
                    let end = self.context().unexpected_token(
                        None,
                        [Token::Comma, closing].iter().delim("|"),
                        true,
                    );
                    return Node(args, self.context().merge_nodes(start, end));
                }
            }
        }
    }

    pub fn parse_arguments(
        &mut self,
        hint: ArgumentsTypeHint<'a, '_, L>,
        fallback_node: NodeId<'a>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
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
}
