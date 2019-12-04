use std::fmt;

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

impl fmt::Display for Type {
    /// Convert Type to string representation
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Byte => write!(f, "byte"),
            Type::Char => write!(f, "char"),
            Type::Double => write!(f, "double"),
            Type::Float => write!(f, "float"),
            Type::Int => write!(f, "int"),
            Type::Long => write!(f, "long"),
            // we can be certain that the classname will be ASCII
            // todo: dont just automatically strip the first 10 chars (java.lang.)
            Type::ClassName(s) => write!(
                f,
                "{}",
                if s.starts_with("java.lang.") {
                    unsafe { String::from_utf8_unchecked(s.as_bytes()[10..].to_owned()) }
                } else {
                    s.to_owned()
                }
            ),
            Type::Short => write!(f, "short"),
            Type::Boolean => write!(f, "boolean"),
            Type::Reference(t) => write!(f, "{}[]", t),
            Type::Void => write!(f, "void"),
        }
    }
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
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg(v) => write!(f, "-{}", v),
            UnaryOp::ArrayLength(v) => write!(f, "{}.length", v),
            UnaryOp::PlusPlus(v) => writeln!(f, "{}++;", v),
            UnaryOp::MinusMinus(v) => writeln!(f, "{}--;", v),
        }
    }
}

impl UnaryOp {
    pub(crate) fn ty(&self) -> Type {
        match self {
            Self::Neg(s) | Self::ArrayLength(s) | Self::PlusPlus(s) | Self::MinusMinus(s) => s.ty(),
        }
    }
}
