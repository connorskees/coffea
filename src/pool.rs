#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PoolKind {
    /// Used to represent a class or an interface
    Class(u16),

    /// Used to represent a class field
    FieldRef {
        /// Index of the class containing this field
        class_index: u16,

        /// Index of the name and type of this field
        name_and_type_index: u16,
    },

    /// Used to represent a class method
    MethodRef {
        /// Index of the class containing this method
        class_index: u16,

        /// Index of the name and return type of this method
        name_and_type_index: u16,
    },

    /// Used to represent an interface method
    InterfaceMethodRef {
        /// Index of the class or interface containing this method
        class_index: u16,

        /// Index of the name and return type of this method
        name_and_type_index: u16,
    },

    /// Used to represent constant String objects
    String(u16),

    /// Used to represent constant `int`s
    Integer(u32),

    /// Used to represent constant `float`s
    Float { bytes: u32 },

    /// Used to represent constant `long`s
    Long(i64),

    /// Used to represent constant `double`s
    Double { high_bytes: u32, low_bytes: u32 },

    /// Represents a field or method without indicating
    /// to which class or interface it belongs
    NameAndType {
        /// Index of the field or method name
        name_index: u16,

        /// Index of the field or method descriptor
        descriptor_index: u16,
    },

    /// A UTF-8 string. Used to represent names of fields, classes, methods, etc.
    Utf8(String),

    /// Represents a handle to a method
    /// (I think this is like a function pointer?)
    MethodHandle {
        reference_kind: MethodReferenceHandleKind,
        reference_index: u16,
    },

    /// Represents the argument and return types of a method
    MethodType {
        /// Index of the method descriptor
        descriptor_index: u16,
    },

    InvokeDynamic {
        boostrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum MethodReferenceHandleKind {
    ///	getfield C.f:T
    GetField = 1,
    ///	getstatic C.f:T
    GetStatic = 2,
    ///	putfield C.f:T
    PutField = 3,
    ///	putstatic C.f:T
    PutStatic = 4,
    ///	invokevirtual C.m:(A*)T
    InvokeVirtual = 5,
    ///	invokestatic C.m:(A*)T
    InvokeStatic = 6,
    ///	invokespecial C.m:(A*)T
    InvokeSpecial = 7,
    ///	new C; dup; invokespecial C.<init>:(A*)void
    NewInvokeSpecial = 8,
    ///	invokeinterface C.m:(A*)T
    InvokeInterface = 9,
}

impl MethodReferenceHandleKind {
    pub fn from_u8(n: u8) -> Self {
        match n {
            1 => Self::GetField,
            2 => Self::GetStatic,
            3 => Self::PutField,
            4 => Self::PutStatic,
            5 => Self::InvokeVirtual,
            6 => Self::InvokeStatic,
            7 => Self::InvokeSpecial,
            8 => Self::NewInvokeSpecial,
            9 => Self::InvokeInterface,
            _ => todo!("invalid method reference handle kind"),
        }
    }
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
    pub fn method_handle(reference_kind: u8, reference_index: u16) -> PoolKind {
        let reference_kind = MethodReferenceHandleKind::from_u8(reference_kind);
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
