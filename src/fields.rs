use crate::attributes::AttributeInfo;

#[derive(Debug)]
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

#[derive(Debug)]
pub struct FieldInfo {
    pub access_flags: FieldAccessFlags,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attribute_info: Vec<AttributeInfo>,
}
