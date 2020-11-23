use std::{
    borrow::{Borrow, Cow},
    fmt,
    io::{self, Write},
};

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
        c => {
            let mut name = c.to_string();
            name.push_str(&cc.collect::<String>());
            Type::ClassName(name)
        }
    })
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub(crate) enum BinaryOp {
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
pub(crate) enum UnaryOp {
    Neg,
    ArrayLength,
    PlusPlus,
    MinusMinus,
    Negate,
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum UnaryOpPosition {
    Prefix,
    Postfix,
}

impl UnaryOp {
    fn position(&self) -> UnaryOpPosition {
        match self {
            UnaryOp::Neg => UnaryOpPosition::Prefix,
            UnaryOp::ArrayLength => UnaryOpPosition::Postfix,
            UnaryOp::PlusPlus => UnaryOpPosition::Postfix,
            UnaryOp::MinusMinus => UnaryOpPosition::Postfix,
            UnaryOp::Negate => UnaryOpPosition::Prefix,
        }
    }

    pub fn is_prefix(&self) -> bool {
        self.position() == UnaryOpPosition::Prefix
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::ArrayLength => write!(f, ".length"),
            UnaryOp::PlusPlus => writeln!(f, "++;"),
            UnaryOp::MinusMinus => writeln!(f, "--;"),
            UnaryOp::Negate => write!(f, "!"),
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

pub(crate) fn double_to_f64(high_bytes: u32, low_bytes: u32) -> f64 {
    let bits: u64 = (u64::from(high_bytes) << 32) + u64::from(low_bytes);
    match bits {
        0x7ff0_0000_0000_0000 => std::f64::INFINITY,
        0xfff0_0000_0000_0000 => std::f64::NEG_INFINITY,
        0x7ff0_0000_0000_0001..=0x7fff_ffff_ffff_ffff
        | 0xfff0_0000_0000_0001..=0xffff_ffff_ffff_ffff => std::f64::NAN,
        _ => {
            let a = high_bytes.to_be_bytes();
            let b = low_bytes.to_be_bytes();
            f64::from_be_bytes([a[0], a[1], a[2], a[3], b[0], b[1], b[2], b[3]])
        }
    }
}

pub(crate) fn float_to_f32(bytes: u32) -> f32 {
    match bytes {
        0x7f80_0000 => std::f32::INFINITY,
        0xff80_0000 => std::f32::NEG_INFINITY,
        0x7f80_0001..=0x7fff_ffff | 0xff80_0001..=0xffff_ffff => std::f32::NAN,
        _ => f32::from_be_bytes(bytes.to_be_bytes()),
    }
}
