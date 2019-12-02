use crate::attributes::Attribute;
use crate::common::Type;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldAccessFlags {
    is_public: bool,
    is_private: bool,
    is_protected: bool,
    is_static: bool,
    is_final: bool,
    is_volatile: bool,
    is_transient: bool,
    is_synthetic: bool,
    is_enum: bool,
}

#[derive(Debug)]
pub struct FieldDescriptor {
    pub ty: Type,
}

impl FieldDescriptor {
    /// Parse field descriptor from str
    pub fn new<S: std::fmt::Debug + AsRef<str>>(s: S) -> FieldDescriptor {
        let mut chars = s.as_ref().chars();
        let ty = FieldDescriptor::eat_type(&mut chars).expect("found no field descriptor type");

        FieldDescriptor { ty }
    }

    fn eat_type<'a>(cc: &mut std::str::Chars<'a>) -> Option<Type> {
        if let Some(c) = cc.next() {
            Some(match c {
                'B' => Type::Byte,
                'C' => Type::Char,
                'D' => Type::Double,
                'F' => Type::Float,
                'I' => Type::Int,
                'J' => Type::Long,
                'L' => {
                    let mut name = String::new();
                    while let Some(c2) = cc.next() {
                        if c2 == ';' {
                            break;
                        }
                        name.push(c2);
                    }
                    Type::ClassName(name)
                }
                'S' => Type::Short,
                'Z' => Type::Boolean,
                'V' => Type::Void,
                '[' => {
                    let t = match FieldDescriptor::eat_type(cc) {
                        Some(t) => t,
                        None => unimplemented!(),
                    };
                    Type::Reference(Box::new(t))
                }
                _ => Type::ClassName(cc.collect()),
            })
        } else {
            None
        }
    }
}

impl FieldAccessFlags {
    pub const PUBLIC: u16 = 0x0001;
    pub const PRIVATE: u16 = 0x0002;
    pub const PROTECTED: u16 = 0x0004;
    pub const STATIC: u16 = 0x0008;
    pub const FINAL: u16 = 0x0010;
    pub const VOLATILE: u16 = 0x0040;
    pub const TRANSIENT: u16 = 0x0080;
    pub const SYNTHETIC: u16 = 0x1000;
    pub const ENUM: u16 = 0x4000;

    #[must_use]
    pub const fn from_u16(n: u16) -> FieldAccessFlags {
        FieldAccessFlags {
            is_public: (n & FieldAccessFlags::PUBLIC) != 0,
            is_private: (n & FieldAccessFlags::PRIVATE) != 0,
            is_protected: (n & FieldAccessFlags::PROTECTED) != 0,
            is_static: (n & FieldAccessFlags::STATIC) != 0,
            is_final: (n & FieldAccessFlags::FINAL) != 0,
            is_volatile: (n & FieldAccessFlags::VOLATILE) != 0,
            is_transient: (n & FieldAccessFlags::TRANSIENT) != 0,
            is_synthetic: (n & FieldAccessFlags::SYNTHETIC) != 0,
            is_enum: (n & FieldAccessFlags::ENUM) != 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldInfo {
    pub access_flags: FieldAccessFlags,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attribute_info: Vec<Attribute>,
}
