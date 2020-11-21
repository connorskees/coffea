use std::io;

use crate::{
    attributes::Attribute,
    common::{parse_single_type, Type},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldInfo {
    pub access_flags: FieldAccessFlags,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attribute_info: Vec<Attribute>,
}

#[derive(Debug)]
pub struct FieldDescriptor {
    pub ty: Type,
}

impl FieldDescriptor {
    /// Parse field descriptor from str
    pub fn new(s: &str) -> FieldDescriptor {
        let mut chars = s.chars();
        let ty = parse_single_type(&mut chars).expect("found no field descriptor type");

        FieldDescriptor { ty }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldAccessFlags {
    pub is_public: bool,
    pub is_private: bool,
    pub is_protected: bool,
    pub is_static: bool,
    pub is_final: bool,
    pub is_volatile: bool,
    pub is_transient: bool,
    pub is_synthetic: bool,
    pub is_enum: bool,
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

    pub fn write(&self, buf: &mut dyn io::Write) -> io::Result<()> {
        // todo: other fields
        let mut has_written = false;
        if self.is_public {
            buf.write_all(b"public")?;
            has_written = true;
        }

        if self.is_static {
            if has_written {
                buf.write_all(b" ")?;
            }
            has_written = true;
            buf.write_all(b"static")?;
        }

        if self.is_final {
            if has_written {
                buf.write_all(b" ")?;
            }
            // has_written = true;
            buf.write_all(b"final")?;
        }

        if has_written {
            buf.write_all(b" ")?;
        }

        Ok(())
    }
}
