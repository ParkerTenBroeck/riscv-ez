use super::Number;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    LPar,
    RPar,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    Plus,
    Minus,
    Star,
    Slash,
    Ampersand,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    ShiftLeft,
    ShiftRight,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Comma,

    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equals,
    NotEquals,

    NewLine,

    Assignment,

    ModuloEq,
    Percent,
    DivideEq,
    TimesEq,
    MinusEq,
    PlusEq,
    SmallRightArrow,
    BigRightArrow,
    OrEq,
    AndEq,
    XorEq,
    ShiftRightEq,
    ShiftLeftEq,

    PreProcessorTag(&'a str),
    Label(&'a str),
    Ident(&'a str),

    StringLiteral(&'a str),
    NumericLiteral(Number<'a>),
    CharLiteral(&'a str),
    FalseLiteral,
    TrueLiteral,

    SingleLineComment(&'a str),
    MultiLineComment(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<T> {
    pub span: Span,
    pub val: T,
}

impl<T> Spanned<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self { val, span }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned {
            span: self.span,
            val: f(self.val),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: u32,
    pub col: u32,
    pub offset: u32,
    pub len: u32,
}

impl Span {
    pub const fn empty() -> Span {
        Span {
            line: 0,
            col: 0,
            offset: 0,
            len: 0,
        }
    }

    pub fn start_end(start: super::Position, end: super::Position) -> Self {
        Span {
            line: start.line as u32,
            col: start.col as u32,
            offset: start.offset as u32,
            len: (end.offset - start.offset) as u32,
        }
    }

    pub fn immediately_after(&self) -> Span {
        Span {
            line: self.line,
            //TODO this assumes ascii
            col: self.col + self.len,
            offset: self.offset + self.len,
            len: 1,
        }
    }

    pub fn combine(&self, other: Span) -> Span {
        let send = self.len + self.offset;
        let oend = other.len + other.offset;
        let offset = self.offset.min(other.offset);
        Span {
            line: self.line.min(other.line),
            col: self.col.min(other.col),
            offset,
            len: if send > oend {
                send - offset
            } else {
                oend - offset
            },
        }
    }
}
