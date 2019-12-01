#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PoolKind {
    Class(u16),
    FieldRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    MethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    InterfaceMethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    String(u16),
    Integer(u32),
    Float {
        bytes: u32,
    },
    Long(i64),
    Double {
        high_bytes: u32,
        low_bytes: u32,
    },
    NameAndType {
        name_index: u16,
        descriptor_index: u16,
    },
    Utf8(String),
    MethodHandle {
        reference_kind: u8,
        reference_index: u16,
    },
    MethodType {
        descriptor_index: u16,
    },
    InvokeDynamic {
        boostrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
}

impl PoolKind {
    #[must_use]
    pub const fn class(name_index: u16) -> PoolKind {
        PoolKind::Class(name_index)
    }

    #[must_use]
    pub const fn field_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::FieldRef {
            class_index,
            name_and_type_index,
        }
    }

    #[must_use]
    pub const fn method_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::MethodRef {
            class_index,
            name_and_type_index,
        }
    }

    #[must_use]
    pub const fn interface_method_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::InterfaceMethodRef {
            class_index,
            name_and_type_index,
        }
    }

    #[must_use]
    pub const fn string(string_index: u16) -> PoolKind {
        PoolKind::String(string_index)
    }

    #[must_use]
    pub const fn integer(bytes: u32) -> PoolKind {
        PoolKind::Integer(bytes)
    }

    #[must_use]
    pub const fn float(bytes: u32) -> PoolKind {
        PoolKind::Float { bytes }
    }

    #[must_use]
    pub const fn long(val: i64) -> PoolKind {
        PoolKind::Long(val)
    }

    #[must_use]
    pub const fn double(high_bytes: u32, low_bytes: u32) -> PoolKind {
        PoolKind::Double {
            high_bytes,
            low_bytes,
        }
    }

    #[must_use]
    pub const fn name_and_type(name_index: u16, descriptor_index: u16) -> PoolKind {
        PoolKind::NameAndType {
            name_index,
            descriptor_index,
        }
    }

    #[must_use]
    pub fn utf8(bytes: &[u8]) -> PoolKind {
        PoolKind::Utf8(std::str::from_utf8(bytes).unwrap().to_owned())
    }

    #[must_use]
    pub const fn method_handle(reference_kind: u8, reference_index: u16) -> PoolKind {
        PoolKind::MethodHandle {
            reference_kind,
            reference_index,
        }
    }

    #[must_use]
    pub const fn method_type(descriptor_index: u16) -> PoolKind {
        PoolKind::MethodType { descriptor_index }
    }

    #[must_use]
    pub const fn invoke_dynamic(
        boostrap_method_attr_index: u16,
        name_and_type_index: u16,
    ) -> PoolKind {
        PoolKind::InvokeDynamic {
            boostrap_method_attr_index,
            name_and_type_index,
        }
    }
}
