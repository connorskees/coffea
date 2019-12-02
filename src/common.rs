use std::fmt;

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
    #[must_use]
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
            Type::ClassName(s) => write!(f, "{}", unsafe {
                String::from_utf8_unchecked(s.as_bytes()[10..].to_owned())
            }),
            Type::Short => write!(f, "short"),
            Type::Boolean => write!(f, "boolean"),
            Type::Reference(t) => write!(f, "{}[]", t),
            Type::Void => write!(f, "void"),
        }
    }
}
