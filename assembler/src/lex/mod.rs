pub mod number;
pub mod sstr;
pub mod token;

pub use number::*;
use std::{iter::Peekable, str::Chars};
pub use token::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(super) struct Position {
    offset: usize,
    line: usize,
    col: usize,
}

type LexerResult<'a> = Result<Spanned<Token<'a>>, Box<Spanned<LexError<'a>>>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexError<'a> {
    InvalidChar(char),
    UnclosedCharLiteral,
    UnclosedMultiLineComment,
    InvalidEscape(&'a str),
    UnfinishedEscapeSequence(&'a str),
    UnclosedStringLiteral,
    EmptyExponent,
    InvalidBase2Digit(char),
    NoNumberAfterBasePrefix,
    NumberParseError(NumberError),
}

impl<'a> std::fmt::Display for LexError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::InvalidChar(char) => write!(f, "invalid character {char:?}"),
            LexError::UnclosedCharLiteral => write!(f, "unclosed char literal"),
            LexError::UnclosedMultiLineComment => write!(f, "unclosed multi line comment"),
            LexError::InvalidEscape(escape) => write!(f, "invalid escape, {escape:?}"),
            LexError::UnfinishedEscapeSequence(escape) => {
                write!(f, "unfinished escape sequence {escape:?}")
            }
            LexError::UnclosedStringLiteral => write!(f, "unclosed string literal"),
            LexError::EmptyExponent => write!(f, "empty exponent"),
            LexError::InvalidBase2Digit(digit) => write!(f, "invalid base 2 digit {digit:?}"),
            LexError::NoNumberAfterBasePrefix => write!(f, "no number after base prefix"),
            LexError::NumberParseError(err) => write!(f, "{err}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum EscapeReturn {
    String,
    Char,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum State {
    Default,

    Plus,
    Minus,
    Times,
    Divide,
    Mod,
    Equal,
    Gt,
    GtGt,
    Lt,
    LtLt,
    Not,
    Or,
    And,
    Xor,

    Ident,

    SingleLine(u8),
    MultiLine(u16),
    MultiLineOpen1(u16),
    MultiLineClose1(u16),

    CharLiteral,

    String,

    EscapeStart(EscapeReturn),

    Eof,

    NumericStartZero,
    NumericStart,
    NumericDecimal,
    NumericDecimalNumberE,
    NumericDecimalNumberENumber,

    NumericBaseStart,
    NumericDecimalNumberEPM,
    NumericBase,
    NumericSuffix,

    PreProcessorTag,
}

pub struct Lexer<'a> {
    str: &'a str,
    chars: Peekable<Chars<'a>>,
    state: State,

    start: Position,
    current: Position,
    escape_start: Position,

    numeric_start: usize,
    type_hint: TypeHint,
    suffix_start: usize,

    include_comments: bool,
}

fn ident(ident: &str) -> Token {
    match ident {
        "true" => Token::TrueLiteral,
        "false" => Token::FalseLiteral,
        o => Token::Ident(o),
    }
}

impl<'a> Lexer<'a> {
    pub fn new(str: &'a str) -> Self {
        Self {
            str,
            chars: str.chars().peekable(),
            state: State::Default,
            start: Position::default(),
            current: Position::default(),
            escape_start: Position::default(),
            include_comments: false,
            numeric_start: 0,
            suffix_start: 0,
            type_hint: TypeHint::Int,
        }
    }

    pub fn include_comments(mut self) -> Self {
        self.include_comments = true;
        self
    }
}

fn ident_start(char: char) -> bool {
    char.is_alphabetic() || char == '_' || char == '$' || char == '?' || char == '.'
}

fn ident_continue(char: char) -> bool {
    char.is_alphanumeric()
        || char == '_'
        || char == '$'
        || char == '#'
        || char == '@'
        || char == '~'
        || char == '?'
        || char == '.'
}

impl<'a> Iterator for Lexer<'a> {
    type Item = LexerResult<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let update_start_on_error = true;

        let mut ret = None;
        let ok_ret_state = State::Default;
        let err_ret_state = State::Default;
        loop {
            let c = self.chars.peek().copied();
            let mut consume = true;

            let processing = if let Some(char) = c {
                let mut tmp = self.current;
                tmp.offset += char.len_utf8();
                if char == '\n' {
                    tmp.line += 1;
                    tmp.col = 0;
                } else {
                    tmp.col += 1;
                }
                tmp
            } else {
                self.current
            };

            macro_rules! eof_none {
                ($expr:expr) => {
                    if let Some(char) = $expr {
                        char
                    } else {
                        self.state = State::Eof;
                        return None;
                    }
                };
            }

            macro_rules! unconsume_ret {
                ($sel:ident, $expr:expr) => {{
                    consume = false;
                    ret = Some($expr);
                }};
            }

            match self.state {
                State::Default => match eof_none!(c) {
                    '\n' => ret = Some(Ok(Token::NewLine)),

                    ';' => self.state = State::SingleLine(1),
                    '#' => self.state = State::PreProcessorTag,
                    '|' => self.state = State::Or,
                    '^' => self.state = State::Xor,
                    '/' => self.state = State::Divide,
                    '%' => self.state = State::Mod,
                    '-' => self.state = State::Minus,
                    '+' => self.state = State::Plus,
                    '*' => self.state = State::Times,
                    '=' => self.state = State::Equal,
                    '<' => self.state = State::Lt,
                    '>' => self.state = State::Gt,
                    '!' => self.state = State::Not,
                    '&' => self.state = State::And,
                    '"' => self.state = State::String,
                    '\'' => self.state = State::CharLiteral,

                    '(' => ret = Some(Ok(Token::LPar)),
                    ')' => ret = Some(Ok(Token::RPar)),
                    '{' => ret = Some(Ok(Token::LBrace)),
                    '}' => ret = Some(Ok(Token::RBrace)),
                    '[' => ret = Some(Ok(Token::LBracket)),
                    ']' => ret = Some(Ok(Token::RBracket)),
                    '~' => ret = Some(Ok(Token::BitwiseNot)),
                    ',' => ret = Some(Ok(Token::Comma)),

                    '0' => {
                        self.numeric_start = self.current.offset;
                        self.state = State::NumericStartZero;
                    }
                    '1'..='9' => {
                        self.numeric_start = self.current.offset;
                        self.state = State::NumericStart;
                    }

                    c if c.is_whitespace() => self.start = processing,
                    c if ident_start(c) => self.state = State::Ident,

                    c => ret = Some(Err(LexError::InvalidChar(c))),
                },
                State::Plus => match c {
                    Some('=') => ret = Some(Ok(Token::PlusEq)),
                    _ => unconsume_ret!(self, Ok(Token::Plus)),
                },
                State::Minus => match c {
                    Some('>') => ret = Some(Ok(Token::SmallRightArrow)),
                    Some('=') => ret = Some(Ok(Token::MinusEq)),
                    _ => unconsume_ret!(self, Ok(Token::Minus)),
                },
                State::Times => match c {
                    Some('=') => ret = Some(Ok(Token::TimesEq)),
                    _ => unconsume_ret!(self, Ok(Token::Star)),
                },
                State::Divide => match c {
                    Some('=') => ret = Some(Ok(Token::DivideEq)),
                    Some('/') => self.state = State::SingleLine(2),
                    Some('*') => self.state = State::MultiLine(0),
                    _ => unconsume_ret!(self, Ok(Token::Slash)),
                },
                State::Mod => match c {
                    Some('=') => ret = Some(Ok(Token::ModuloEq)),
                    _ => unconsume_ret!(self, Ok(Token::Percent)),
                },
                State::Equal => match c {
                    Some('>') => ret = Some(Ok(Token::BigRightArrow)),
                    Some('=') => ret = Some(Ok(Token::Equals)),
                    _ => unconsume_ret!(self, Ok(Token::Assignment)),
                },
                State::Gt => match c {
                    Some('=') => ret = Some(Ok(Token::GreaterThanEq)),
                    Some('>') => self.state = State::GtGt,
                    _ => unconsume_ret!(self, Ok(Token::GreaterThan)),
                },
                State::GtGt => match c {
                    Some('=') => ret = Some(Ok(Token::ShiftRightEq)),
                    _ => unconsume_ret!(self, Ok(Token::ShiftRight)),
                },
                State::Lt => match c {
                    Some('=') => ret = Some(Ok(Token::LessThanEq)),
                    Some('<') => self.state = State::LtLt,
                    _ => unconsume_ret!(self, Ok(Token::LessThan)),
                },
                State::LtLt => match c {
                    Some('=') => ret = Some(Ok(Token::ShiftLeftEq)),
                    _ => unconsume_ret!(self, Ok(Token::ShiftLeft)),
                },
                State::Not => match c {
                    Some('=') => ret = Some(Ok(Token::NotEquals)),
                    _ => unconsume_ret!(self, Ok(Token::LogicalNot)),
                },
                State::Or => match c {
                    Some('=') => ret = Some(Ok(Token::OrEq)),
                    Some('|') => ret = Some(Ok(Token::LogicalOr)),
                    _ => unconsume_ret!(self, Ok(Token::BitwiseOr)),
                },
                State::And => match c {
                    Some('=') => ret = Some(Ok(Token::AndEq)),
                    Some('&') => ret = Some(Ok(Token::LogicalAnd)),
                    _ => unconsume_ret!(self, Ok(Token::Ampersand)),
                },
                State::Xor => match c {
                    Some('=') => ret = Some(Ok(Token::XorEq)),
                    _ => unconsume_ret!(self, Ok(Token::BitwiseXor)),
                },
                State::Ident => match c {
                    Some(c) if ident_continue(c) => {}
                    Some(':') => {
                        ret = Some(Ok(Token::Label(
                            &self.str[self.start.offset..self.current.offset],
                        )))
                    }
                    _ => unconsume_ret!(
                        self,
                        Ok(ident(&self.str[self.start.offset..self.current.offset]))
                    ),
                },
                State::PreProcessorTag => match c {
                    Some(c) if ident_continue(c) => {}
                    _ => unconsume_ret!(
                        self,
                        Ok(Token::PreProcessorTag(
                            &self.str[self.start.offset + "#".len()..self.current.offset]
                        ))
                    ),
                },
                State::CharLiteral => match c {
                    Some('\'') => {
                        ret = Some(Ok(Token::CharLiteral(
                            self.str[self.start.offset + 1..self.current.offset].into(),
                        )))
                    }
                    Some('\n') => ret = Some(Err(LexError::UnclosedCharLiteral)),
                    Some('\\') => {
                        self.escape_start = self.current;
                        self.state = State::EscapeStart(EscapeReturn::Char);
                    }
                    Some(_) => {}
                    None => ret = Some(Err(LexError::UnclosedCharLiteral)),
                },

                State::String => match c {
                    Some('"') => {
                        ret = Some(Ok(Token::StringLiteral(
                            self.str[self.start.offset + 1..self.current.offset].into(),
                        )))
                    }
                    Some('\\') => {
                        self.escape_start = self.current;
                        self.state = State::EscapeStart(EscapeReturn::String);
                    }
                    Some(_) => {}
                    None => ret = Some(Err(LexError::UnclosedStringLiteral)),
                },
                State::EscapeStart(ret_state) => {
                    self.state = match ret_state {
                        EscapeReturn::String => State::String,
                        EscapeReturn::Char => State::CharLiteral,
                    };
                }
                State::SingleLine(start) => match c {
                    Some('\n') => {
                        unconsume_ret!(
                            self,
                            Ok(Token::SingleLineComment(
                                self.str[self.start.offset + start as usize..self.current.offset]
                                    .into(),
                            ))
                        );
                    }
                    Some(_) => {}
                    None => {
                        ret = Some(Ok(Token::SingleLineComment(
                            self.str[self.start.offset + start as usize..self.current.offset]
                                .into(),
                        )))
                    }
                },
                State::MultiLine(indent) => match c {
                    Some('/') => self.state = State::MultiLineOpen1(indent),
                    Some('*') => self.state = State::MultiLineClose1(indent),
                    Some(_) => {}
                    None => ret = Some(Err(LexError::UnclosedMultiLineComment)),
                },
                State::MultiLineOpen1(indent) => match c {
                    Some('*') => self.state = State::MultiLine(indent + 1),
                    Some(_) => {
                        consume = false;
                        self.state = State::MultiLine(indent)
                    }
                    None => ret = Some(Err(LexError::UnclosedMultiLineComment)),
                },
                State::MultiLineClose1(indent) => match (indent, c) {
                    (0, Some('/')) => {
                        ret = Some(Ok(Token::MultiLineComment(
                            self.str[self.start.offset + ('*'.len_utf8() + '/'.len_utf8())
                                ..processing.offset - ('*'.len_utf8() + '/'.len_utf8())]
                                .into(),
                        )))
                    }
                    (indent, Some('/')) => self.state = State::MultiLine(indent - 1),
                    (_, None) => ret = Some(Err(LexError::UnclosedMultiLineComment)),
                    _ => {
                        consume = false;
                        self.state = State::MultiLine(indent);
                    }
                },

                State::NumericStart => match c {
                    Some('0'..='9') => {}
                    Some('.') => self.state = State::NumericDecimal,
                    Some('e') => {
                        self.state = State::NumericDecimalNumberE;
                    }
                    Some('_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Int;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Int,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericStartZero => match c {
                    Some('b') => {
                        self.numeric_start = processing.offset;
                        self.state = State::NumericBaseStart;
                        self.type_hint = TypeHint::Bin;
                    }
                    Some('o') => {
                        self.numeric_start = processing.offset;
                        self.state = State::NumericBaseStart;
                        self.type_hint = TypeHint::Oct;
                    }
                    Some('x') => {
                        self.numeric_start = processing.offset;
                        self.state = State::NumericBaseStart;
                        self.type_hint = TypeHint::Hex;
                    }
                    Some('0'..='9') => {
                        self.state = State::NumericStart;
                    }
                    Some('.') => self.state = State::NumericDecimal,
                    Some('_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Int;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Int,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericDecimal => match c {
                    Some('0'..='9') => {}
                    Some('e') => {
                        self.state = State::NumericDecimalNumberE;
                    }
                    Some('_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Float;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Float,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericDecimalNumberE => match c {
                    Some('0'..='9') => {
                        self.state = State::NumericDecimalNumberENumber;
                    }
                    Some('+' | '-') => {
                        self.state = State::NumericDecimalNumberEPM;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(LexError::EmptyExponent));
                    }
                },
                State::NumericDecimalNumberEPM => match c {
                    Some('0'..='9') => {
                        self.state = State::NumericDecimalNumberENumber;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(LexError::EmptyExponent));
                    }
                },
                State::NumericDecimalNumberENumber => match c {
                    Some('0'..='9' | '_') => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.type_hint = TypeHint::Float;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                TypeHint::Float,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericBaseStart => match c {
                    Some('0'..='9' | 'a'..='z' | 'A'..='Z') => {
                        self.state = State::NumericBase;
                    }
                    Some('_') => {}
                    _ => {
                        consume = false;
                        ret = Some(Err(LexError::NoNumberAfterBasePrefix))
                    }
                },
                State::NumericBase => match c {
                    Some('0'..='9') => {}
                    Some('_') => {}
                    Some('a'..='f' | 'A'..='F') if self.type_hint == TypeHint::Hex => {}
                    Some('a'..='z' | 'A'..='Z') => {
                        self.suffix_start = self.current.offset;
                        self.state = State::NumericSuffix;
                    }
                    _ => {
                        consume = false;
                        ret = Some(
                            Number::new(
                                &self.str[self.numeric_start..self.current.offset],
                                self.type_hint,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::NumericSuffix => match c {
                    Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') => {}
                    _ => {
                        consume = false;
                        let len = self.suffix_start - self.numeric_start;
                        ret = Some(
                            Number::new_with_suffix(
                                &self.str[self.numeric_start..self.current.offset],
                                len,
                                self.type_hint,
                            )
                            .map(Token::NumericLiteral)
                            .map_err(LexError::NumberParseError),
                        );
                    }
                },
                State::Eof => return None,
            }

            if consume {
                self.chars.next();
                self.current = processing;
            }

            if let Some(ret_res) = ret {
                match ret_res {
                    Ok(token) => {
                        let meta = Span::start_end(self.start, self.current);
                        self.start = self.current;
                        self.state = ok_ret_state;
                        if matches!(
                            token,
                            Token::MultiLineComment(_) | Token::SingleLineComment(_)
                        ) && !self.include_comments
                        {
                            ret = None;
                            continue;
                        }
                        return Some(Ok(Spanned::new(token, meta)));
                    }
                    Err(err) => {
                        let meta = Span::start_end(self.start, self.current);
                        if update_start_on_error {
                            self.start = self.current;
                        }
                        self.state = err_ret_state;
                        return Some(Err(Box::new(Spanned::new(err, meta))));
                    }
                }
            }
        }
    }
}
