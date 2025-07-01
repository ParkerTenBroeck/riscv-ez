use crate::assembler::{Assembler, Constant, ParsedArgument};
use crate::context::{Node, NodeId};
use crate::lex::{Number, Token};

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum NumericTypeHint {
    None,
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
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum ArgumentTypeHint {
    Numeric(NumericTypeHint),
    String,
    Char,
    Ident,
    Label,
    LabelParing,
    None,
}

impl<'a> Assembler<'a> {
    fn parse_numeric_literal(&mut self, num: Number<'a>, n: NodeId<'a>) -> Node<'a, Constant<'a>> {
        let (suffix, radix) = match num.get_hint() {
            crate::lex::TypeHint::Float => (num.get_suffix().unwrap_or("f32"), 10),
            crate::lex::TypeHint::Hex => (num.get_suffix().unwrap_or("i32"), 16),
            crate::lex::TypeHint::Bin => (num.get_suffix().unwrap_or("i32"), 2),
            crate::lex::TypeHint::Int => (num.get_suffix().unwrap_or("i32"), 10),
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

        Node(
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
            },
            n,
        )
    }

    fn parse_argument(&mut self) -> Option<Node<'a, ParsedArgument<'a>>> {
        match self.peek() {
            Some(Node(Token::NumericLiteral(num), n)) => {
                self.next();
                Some(
                    self.parse_numeric_literal(num, n)
                        .map(ParsedArgument::Constant),
                )
            }
            Some(Node(Token::Ident(str), n)) => {
                self.next();
                Some(Node(ParsedArgument::Label(str, 0), n))
            }
            Some(Node(Token::StringLiteral(str), n)) => {
                self.next();
                Some(Node(
                    ParsedArgument::Constant(Constant::StringLiteral(
                        self.parse_string_literal(str, n),
                    )),
                    n,
                ))
            }
            Some(Node(Token::CharLiteral(str), n)) => {
                self.next();
                Some(Node(
                    ParsedArgument::Constant(Constant::CharLiteral(
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
    ) -> Result<Vec<Node<'a, ParsedArgument<'a>>>, ()> {
        let mut args = Vec::new();
        while !matches!(self.peek(), Some(Node(Token::NewLine, _)) | None) {
            args.push(self.parse_argument().ok_or(())?);
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
