use crate::attributes::Attribute;

#[derive(Debug)]
pub struct MethodInfo {
    pub access_flags: MethodAccessFlags,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug)]
pub struct MethodAccessFlags {
    is_public: bool,
    is_private: bool,
    is_protected: bool,
    is_static: bool,
    is_final: bool,
    is_synchronized: bool,
    is_bridge: bool,
    is_var_args: bool,
    is_native: bool,
    is_abstract: bool,
    is_strict: bool,
    is_synthetic: bool,
}

impl MethodAccessFlags {
    pub const PUBLIC: u16 = 0x0001;
    pub const PRIVATE: u16 = 0x0002;
    pub const PROTECTED: u16 = 0x0004;
    pub const STATIC: u16 = 0x0008;
    pub const FINAL: u16 = 0x0010;
    pub const SYNCHRONIZED: u16 = 0x0020;
    pub const BRIDGE: u16 = 0x0040;
    pub const VARARGS: u16 = 0x0080;
    pub const NATIVE: u16 = 0x0100;
    pub const ABSTRACT: u16 = 0x0400;
    pub const STRICT: u16 = 0x0800;
    pub const SYNTHETIC: u16 = 0x1000;

    pub fn from_u16(n: u16) -> MethodAccessFlags {
        MethodAccessFlags {
            is_public: (n & MethodAccessFlags::PUBLIC) != 0,
            is_private: (n & MethodAccessFlags::PRIVATE) != 0,
            is_protected: (n & MethodAccessFlags::PROTECTED) != 0,
            is_static: (n & MethodAccessFlags::STATIC) != 0,
            is_final: (n & MethodAccessFlags::FINAL) != 0,
            is_synchronized: (n & MethodAccessFlags::SYNCHRONIZED) != 0,
            is_bridge: (n & MethodAccessFlags::BRIDGE) != 0,
            is_var_args: (n & MethodAccessFlags::VARARGS) != 0,
            is_native: (n & MethodAccessFlags::NATIVE) != 0,
            is_abstract: (n & MethodAccessFlags::ABSTRACT) != 0,
            is_strict: (n & MethodAccessFlags::STRICT) != 0,
            is_synthetic: (n & MethodAccessFlags::SYNTHETIC) != 0,
        }
    }

    pub fn is_public(&self) -> bool {
        self.is_public
    }
    pub fn is_private(&self) -> bool {
        self.is_private
    }
    pub fn is_protected(&self) -> bool {
        self.is_protected
    }
    pub fn is_static(&self) -> bool {
        self.is_static
    }
    pub fn is_final(&self) -> bool {
        self.is_final
    }
    pub fn is_synchronized(&self) -> bool {
        self.is_synchronized
    }
    pub fn is_bridge(&self) -> bool {
        self.is_bridge
    }
    pub fn is_var_args(&self) -> bool {
        self.is_var_args
    }
    pub fn is_native(&self) -> bool {
        self.is_native
    }
    pub fn is_abstract(&self) -> bool {
        self.is_abstract
    }
    pub fn is_strict(&self) -> bool {
        self.is_strict
    }
    pub fn is_synthetic(&self) -> bool {
        self.is_synthetic
    }
}
