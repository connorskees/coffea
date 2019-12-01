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
    pub fn as_string(self) -> String {
        match self {
            Type::Byte => String::from("byte"),
            Type::Char => String::from("char"),
            Type::Double => String::from("double"),
            Type::Float => String::from("float"),
            Type::Int => String::from("int"),
            Type::Long => String::from("long"),
            // we can be certain that the classname will be ASCII
            Type::ClassName(s) => unsafe {
                String::from_utf8_unchecked(s.as_bytes()[10..].to_owned())
            },
            Type::Short => String::from("short"),
            Type::Boolean => String::from("boolean"),
            Type::Reference(t) => format!("{}[]", t.as_string()),
            Type::Void => String::from("void"),
        }
    }
}