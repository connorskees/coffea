
#[derive(Debug)]
pub enum PoolKind {
    Class {
        name_index: u16,
    },
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
    String {
        string_index: u16,
    },
    Integer {
        bytes: u32,
    },
    Float {
        bytes: u32,
    },
    Long {
        high_bytes: u32,
        low_bytes: u32,
    },
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
    pub fn class(name_index: u16) -> PoolKind {
        PoolKind::Class { name_index }
    }

    pub fn field_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::FieldRef {
            class_index,
            name_and_type_index,
        }
    }

    pub fn method_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::MethodRef {
            class_index,
            name_and_type_index,
        }
    }

    pub fn interface_method_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::InterfaceMethodRef {
            class_index,
            name_and_type_index,
        }
    }

    pub fn string(string_index: u16) -> PoolKind {
        PoolKind::String { string_index }
    }

    pub fn integer(bytes: u32) -> PoolKind {
        PoolKind::Integer { bytes }
    }

    pub fn float(bytes: u32) -> PoolKind {
        PoolKind::Float { bytes }
    }

    pub fn long(high_bytes: u32, low_bytes: u32) -> PoolKind {
        PoolKind::Long {
            high_bytes,
            low_bytes,
        }
    }

    pub fn double(high_bytes: u32, low_bytes: u32) -> PoolKind {
        PoolKind::Double {
            high_bytes,
            low_bytes,
        }
    }

    pub fn name_and_type(name_index: u16, descriptor_index: u16) -> PoolKind {
        PoolKind::NameAndType {
            name_index,
            descriptor_index,
        }
    }

    pub fn utf8(bytes: Vec<u8>) -> PoolKind {
        PoolKind::Utf8(std::str::from_utf8(&bytes).unwrap().to_owned())
    }

    pub fn method_handle(reference_kind: u8, reference_index: u16) -> PoolKind {
        PoolKind::MethodHandle {
            reference_kind,
            reference_index,
        }
    }

    pub fn method_type(descriptor_index: u16) -> PoolKind {
        PoolKind::MethodType { descriptor_index }
    }

    pub fn invoke_dynamic(boostrap_method_attr_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::InvokeDynamic {
            boostrap_method_attr_index,
            name_and_type_index,
        }
    }
}
