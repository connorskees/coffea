use std::{
    borrow::{Borrow, Cow},
    fmt,
    io::{self, Write},
};

use crate::StackEntry;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// signed byte
    Byte,
    /// Unicode character code point in the Basic Multilingual Plane, encoded with UTF-16
    Char,
    /// double-precision floating-point value
    Double,
    /// single-precision floating-point value
    Float,
    /// integer
    Int,
    /// long integer
    Long,
    /// an instance of class ClassName
    ClassName(String),
    /// signed short
    Short,
    /// true or false
    Boolean,
    /// one array dimension
    Reference(Box<Type>),
    /// void
    Void,
}

impl Type {
    pub fn as_str(&self) -> Cow<'static, str> {
        match self {
            Type::Byte => Cow::Borrowed("byte"),
            Type::Char => Cow::Borrowed("char"),
            Type::Double => Cow::Borrowed("double"),
            Type::Float => Cow::Borrowed("float"),
            Type::Int => Cow::Borrowed("int"),
            Type::Long => Cow::Borrowed("long"),
            // todo: do we always want to remove `java.lang.`?
            Type::ClassName(s) => Cow::Owned(s.strip_prefix("java/lang/").unwrap_or(s).to_owned()),
            Type::Short => Cow::Borrowed("short"),
            Type::Boolean => Cow::Borrowed("boolean"),
            Type::Reference(t) => Cow::Owned(format!("{}[]", t)),
            Type::Void => Cow::Borrowed("void"),
        }
    }

    /// Helper method for constructing the `java.lang.String` type
    pub fn string() -> Self {
        Type::ClassName("java/lang/String".to_owned())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str().borrow())
    }
}

pub(crate) fn parse_single_type(cc: &mut std::str::Chars<'_>) -> Option<Type> {
    Some(match cc.next()? {
        ')' => return None,
        'B' => Type::Byte,
        'C' => Type::Char,
        'D' => Type::Double,
        'F' => Type::Float,
        'I' => Type::Int,
        'J' => Type::Long,
        'L' => {
            let mut name = String::new();
            while let Some(c) = cc.next() {
                if c == ';' {
                    break;
                }
                name.push(c);
            }
            Type::ClassName(name)
        }
        'S' => Type::Short,
        'Z' => Type::Boolean,
        'V' => Type::Void,
        '[' => {
            let t = match parse_single_type(cc) {
                Some(t) => t,
                None => todo!(),
            };
            Type::Reference(Box::new(t))
        }
        _ => todo!("unknown character"),
    })
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    /// arithmetic shift right
    Shr,
    /// logical shift right
    UShr,
    /// bitwise and
    And,
    Or,
    LogicalAnd,
    LogicalOr,
    Xor,
    InstanceOf,
    Equal,
    NotEqual,
    GreaterThan,
    LessThan,
    GreaterEqualThan,
    LessEqualThan,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
            BinaryOp::Rem => write!(f, "%"),
            BinaryOp::Shl => write!(f, "<<"),
            BinaryOp::Shr => write!(f, ">>"),
            BinaryOp::UShr => write!(f, ">>>"),
            BinaryOp::And => write!(f, "&"),
            BinaryOp::Or => write!(f, "|"),
            BinaryOp::LogicalAnd => write!(f, "&&"),
            BinaryOp::LogicalOr => write!(f, "||"),
            BinaryOp::Xor => write!(f, "^"),
            BinaryOp::InstanceOf => write!(f, "instanceof"),
            BinaryOp::Equal => write!(f, "=="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::GreaterThan => write!(f, ">"),
            BinaryOp::LessThan => write!(f, "<"),
            BinaryOp::GreaterEqualThan => write!(f, ">="),
            BinaryOp::LessEqualThan => write!(f, "<="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg(StackEntry),
    ArrayLength(StackEntry),
    PlusPlus(StackEntry),
    MinusMinus(StackEntry),
    Negate(StackEntry),
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg(v) => write!(f, "-{}", v),
            UnaryOp::ArrayLength(v) => write!(f, "{}.length", v),
            UnaryOp::PlusPlus(v) => writeln!(f, "{}++;", v),
            UnaryOp::MinusMinus(v) => writeln!(f, "{}--;", v),
            UnaryOp::Negate(v) => write!(f, "!{}", v),
        }
    }
}

impl UnaryOp {
    pub(crate) fn ty(&self) -> Type {
        match self {
            Self::Neg(s)
            | Self::Negate(s)
            | Self::ArrayLength(s)
            | Self::PlusPlus(s)
            | Self::MinusMinus(s) => s.ty(),
        }
    }
}

pub(crate) struct Indent {
    buffer: Vec<u8>,
}

impl Indent {
    pub fn new() -> Self {
        Self { buffer: Vec::new() }
    }

    pub fn increase(&mut self) {
        self.buffer.push(b' ');
        self.buffer.push(b' ');
    }

    pub fn decrease(&mut self) {
        self.buffer.pop();
        self.buffer.pop();
    }

    pub fn write(&self, buf: &mut dyn Write) -> io::Result<()> {
        buf.write_all(&self.buffer)
    }
}
