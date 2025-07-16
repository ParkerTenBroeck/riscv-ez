pub mod args;
pub mod value;
pub use value::*;
pub mod base;
pub use base::*;

use crate::assembler::{Assembler, lang::AssemblyLanguage};
use crate::context::{Context, Node, NodeId};
use crate::expression::args::CoercedArgs;
use crate::lex::{Number, Token};
use crate::util::IntoStrDelimable;
use std::marker::PhantomData;

pub type NodeVal<'a, T> = Node<'a, Value<'a, T>>;

pub enum ExprKind {
    Assembler,
    PreProcessor,
    Linker,
}

pub trait ExpressionEvaluatorContext<'a, L: AssemblyLanguage<'a>>: Sized {
    const KIND: ExprKind;

    fn next(&mut self) -> Option<Node<'a, Token<'a>>>;
    fn peek(&mut self) -> Option<Node<'a, Token<'a>>>;
    fn context(&mut self) -> &mut Context<'a> {
        &mut self.asm().state.context
    }
    fn asm(&mut self) -> Assembler<'a, '_, L>;
    fn eval(&mut self) -> ExpressionEvaluator<'a, '_, L, Self> {
        ExpressionEvaluator(self, PhantomData)
    }

    fn expr(&mut self, hint: ValueType<'a, L>) -> Node<'a, Value<'a, L>> {
        ExpressionEvaluator(self, PhantomData).parse_expr(hint)
    }

    fn args(
        &mut self,
        init: NodeId<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        ExpressionEvaluator(self, PhantomData).parse_arguments(init, hint)
    }

    fn args_delim(
        &mut self,
        init: NodeId<'a>,
        start: Token<'a>,
        end: Token<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        ExpressionEvaluator(self, PhantomData).parse_arguments_delim(init, start, end, hint)
    }

    fn coerced<T: CoercedArgs<'a, L>>(&mut self, init: NodeId<'a>) -> Node<'a, T>
    where
        Self: Sized,
    {
        T::coerced_args(self, init)
    }

    fn coerced_delim<T: CoercedArgs<'a, L>>(
        &mut self,
        init: NodeId<'a>,
        start: Token<'a>,
        end: Token<'a>,
    ) -> Node<'a, T>
    where
        Self: Sized,
    {
        T::coerced_args_delim(self, init, start, end)
    }
}

pub struct FuncParamParser<'a, 'b> {
    func: Node<'a, &'a str>,
    func_node: &'b mut Option<NodeId<'a>>,
}

impl<'a, 'b> FuncParamParser<'a, 'b> {
    pub fn func(&self) -> &'a str {
        self.func.0
    }

    pub fn coerced_args<
        A: CoercedArgs<'a, L>,
        L: AssemblyLanguage<'a>,
        T: ExpressionEvaluatorContext<'a, L> + Sized,
    >(
        self,
        ctx: &mut T,
    ) -> Node<'a, A> {
        let args = A::coerced_args_delim(ctx, self.func.1, Token::LPar, Token::RPar);
        *self.func_node = Some(args.1);
        args
    }

    pub fn args<L: AssemblyLanguage<'a>, T: ExpressionEvaluatorContext<'a, L> + Sized>(
        self,
        ctx: &mut T,
    ) -> Node<'a, Vec<Node<'a, Value<'a, L>>>> {
        let args = ctx.args_delim(
            self.func.1,
            Token::LPar,
            Token::RPar,
            ArgumentsTypeHint::None,
        );        
        *self.func_node = Some(args.1);
        args
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
                    let mut loc = None;
                    let res = L::eval_func(
                        self.0,
                        FuncParamParser {
                            func: Node(ident, node),
                            func_node: &mut loc,
                        },
                        hint,
                    );

                    let loc = if let Some(loc) = loc {
                        loc
                    } else {
                        let loc = self.parse_arguments_delim(
                            node,
                            Token::LPar,
                            Token::RPar,
                            ArgumentsTypeHint::None,
                        );
                        self.context().report_error(loc.1, "Function arguments never consumer... This is a error on the developers part");
                        loc.1
                    };

                    Node(res, loc)
                }
                _ => Node(L::parse_ident(self.0, Node(ident, node), hint), node),
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
                L::parse_numeric_literal(self.0, Node(num, node), negated, hint),
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
            Some(Node(Token::LBracket, opening)) => {
                self.next();
                let rhs = self.parse_expr(hint);

                let closing = match self.peek() {
                    Some(Node(Token::RBracket, closing)) => {
                        self.next();
                        closing
                    }
                    t => self.context().unexpected_token(t, Token::RBracket, false),
                };
                let node = self.context().merge_nodes(expr.1, closing);
                return Node(
                    L::eval_index(self.0, node, expr, opening, rhs, closing, hint),
                    node,
                );
            }
            _ => {}
        }

        expr
    }

    fn parse_expr_2(&mut self, hint: ValueType<'a, L>, negated: bool) -> NodeVal<'a, L> {
        match self.peek() {
            Some(Node(Token::Minus, op_node)) => {
                self.next();
                let expr = self.parse_expr_2(hint, !negated);
                let node = self.context().merge_nodes(op_node, expr.1);
                Node(
                    L::eval_unnop(self.0, node, Node(unop::UnOp::Neg, node), expr, hint),
                    node,
                )
            }
            Some(Node(Token::LogicalNot, op_node)) => {
                self.next();
                let expr = self.parse_expr_2(hint, false);
                let node = self.context().merge_nodes(op_node, expr.1);
                Node(
                    L::eval_unnop(self.0, node, Node(unop::UnOp::Not, node), expr, hint),
                    node,
                )
            }
            _ => self.parse_expr_1(hint, negated),
        }
    }

    fn parse_expr_3(&mut self, hint: ValueType<'a, L>) -> NodeVal<'a, L> {
        let expr = self.parse_expr_2(hint, false);
        match self.peek() {
            Some(Node(Token::Ident("as"), as_node)) => {
                self.next();

                let ty = match self.peek() {
                    Some(Node(Token::Ident(ty), node)) => {
                        self.next();
                        Node(ty, node)
                    }

                    t => {
                        self.context().unexpected_token(t, Token::Ident(""), false);
                        return expr;
                    }
                };
                let node = self.context().merge_nodes(expr.1, ty.1);
                Node(L::eval_cast(self.0, node, expr, as_node, ty, hint), node)
            }
            _ => expr,
        }
    }

    fn parse_expr_4(&mut self, hint: ValueType<'a, L>, min_prec: u32) -> NodeVal<'a, L> {
        use Node as N;
        use Token as T;
        use binop::BinOp as BO;
        let mut lhs = self.parse_expr_3(hint);
        loop {
            let op = match self.peek() {
                Some(N(T::Plus, n)) if BO::Add.precedence() >= min_prec => N(BO::Add, n),
                Some(N(T::Minus, n)) if BO::Sub.precedence() >= min_prec => N(BO::Sub, n),
                Some(N(T::Star, n)) if BO::Mul.precedence() >= min_prec => N(BO::Mul, n),
                Some(N(T::Slash, n)) if BO::Div.precedence() >= min_prec => N(BO::Div, n),
                Some(N(T::Percent, n)) if BO::Rem.precedence() >= min_prec => N(BO::Rem, n),
                Some(N(T::LogicalOr, n)) if BO::Or.precedence() >= min_prec => N(BO::Or, n),
                Some(N(T::BitwiseOr, n)) if BO::Or.precedence() >= min_prec => N(BO::Or, n),
                Some(N(T::Ampersand, n)) if BO::And.precedence() >= min_prec => N(BO::And, n),
                Some(N(T::LogicalAnd, n)) if BO::And.precedence() >= min_prec => N(BO::And, n),
                Some(N(T::BitwiseXor, n)) if BO::Xor.precedence() >= min_prec => N(BO::Xor, n),
                Some(N(T::ShiftLeft, n)) if BO::Shl.precedence() >= min_prec => N(BO::Shl, n),
                Some(N(T::ShiftRight, n)) if BO::Shr.precedence() >= min_prec => N(BO::Shr, n),
                Some(N(T::GreaterThan, n)) if BO::Gt.precedence() >= min_prec => N(BO::Gt, n),
                Some(N(T::GreaterThanEq, n)) if BO::Gte.precedence() >= min_prec => N(BO::Gte, n),
                Some(N(T::LessThan, n)) if BO::Lt.precedence() >= min_prec => N(BO::Lt, n),
                Some(N(T::LessThanEq, n)) if BO::Lte.precedence() >= min_prec => N(BO::Lte, n),
                Some(N(T::Equals, n)) if BO::Eq.precedence() >= min_prec => N(BO::Eq, n),
                Some(N(T::NotEquals, n)) if BO::Ne.precedence() >= min_prec => N(BO::Ne, n),
                _ => break,
            };
            self.next();

            let shift = matches!(op.0, BO::Shl | BO::Shr);

            let rhs = self.parse_expr_4(
                if shift { ValueType::U32 } else { hint },
                op.0.precedence() + 1,
            );
            let node = self.context().merge_nodes(lhs.1, rhs.1);
            lhs = Node(L::eval_binop(self.0, node, lhs, op, rhs, hint), node);
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
        init: NodeId<'a>,
        opening: Token<'a>,
        closing: Token<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        let mut args = Vec::new();

        match self.peek() {
            Some(Node(t, _)) if t == opening => {
                self.next();
            }
            t => {
                self.context().unexpected_token(t, opening, true);
            }
        };
        loop {
            match self.peek() {
                Some(Node(t, end)) if t == closing => {
                    self.next();
                    return Node(args, self.context().merge_nodes(init, end));
                }
                Some(_) => {
                    args.push(self.parse_expr(hint[args.len()]));
                    match self.peek() {
                        Some(Node(Token::Comma, _)) => {
                            self.next();
                        }
                        _ => {}
                    }
                }
                None => {
                    let end = self.context().unexpected_token(
                        None,
                        [Token::Comma, closing].iter().delim("|"),
                        true,
                    );
                    return Node(args, self.context().merge_nodes(init, end));
                }
            }
        }
    }

    pub fn parse_arguments(
        &mut self,
        init: NodeId<'a>,
        hint: ArgumentsTypeHint<'a, '_, L>,
    ) -> Node<'a, Vec<NodeVal<'a, L>>> {
        let mut args = Vec::new();

        loop {
            match self.peek() {
                Some(Node(Token::NewLine, _)) | None => {
                    if let Some(Node(_, last)) = args.last().copied() {
                        return Node(args, self.context().merge_nodes(init, last));
                    } else {
                        return Node(args, init);
                    }
                }
                _ => {
                    args.push(self.parse_expr(hint[args.len()]));
                    match self.peek() {
                        Some(Node(Token::Comma, _)) => {
                            self.next();
                        }
                        _ => {}
                    }
                }
            }
        }
    }
}
