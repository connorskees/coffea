use crate::code::Code;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    /// A `ConstantValue` attribute represents the value of a constant
    /// field. There can be no more than one `ConstantValue` attribute
    /// in the attributes table of a given field_info structure. If the
    /// field is static (that is, the ACC_STATIC flag  in the
    /// access_flags item of the field_info structure is set) then the
    /// constant field represented by the field_info structure is assigned
    /// the value referenced by its `ConstantValue` attribute as part of
    /// the initialization of the class or interface declaring the
    /// constant field.
    ///
    /// This occurs prior to the invocation of the class or interface
    /// initialization method of that class or interface.
    ///
    /// If a field_info structure representing a non-static field has a
    /// ConstantValue attribute, then that attribute must silently be ignored.
    ConstantValue {
        const_value_index: u16,
    },

    /// The `Code` attribute is a variable-length attribute in the attributes
    /// table of a method_info structure. A `Code` attribute contains the Java
    /// Virtual Machine instructions and auxiliary information for a single
    /// method, instance initialization method, or class or interface
    /// initialization method.
    ///
    /// If the method is either native or abstract, its method_info structure
    /// must not have a `Code` attribute. Otherwise, its method_info structure
    /// must have exactly one `Code` attribute.
    Code(Code),
    StackMapTable(Vec<FrameType>),
    Exceptions(Vec<u16>),
    InnerClasses(Vec<ClassInfo>),
    EnclosingMethod {
        class_index: u16,
        method_index: u16,
    },
    Synthetic,
    Signature(u16),
    SourceFile(u16),
    SourceDebugExtension(Vec<u8>),
    LineNumberTable(Vec<LineNumberTableEntry>),
    LocalVariableTable(Vec<LocalVariableTableEntry>),
    LocalVariableTypeTable(Vec<LocalVariableTableEntry>),
    Deprecated,
    RuntimeVisibleAnnotations(Vec<Annotation>),
    RuntimeInvisibleAnnotations(Vec<Annotation>),
    RuntimeVisibleParameterAnnotations(Vec<Vec<Annotation>>),
    RuntimeInvisibleParameterAnnotations(Vec<Vec<Annotation>>),
    AnnotationDefault(ElementValue),
    BootstrapMethods(Vec<BootstrapMethod>),
    Other {
        info: Vec<u8>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ExceptionTableEntry {
    /// `start` and `end` are indices into the `code` vec and
    /// indicate the range during which the exception handler is active.
    ///
    /// `start` is an *inclusive* index into the  `code` vec at the position of an the opcode of an instruction.
    pub start: u16,
    /// `end` is an *exclusive* index into the  `code` vec at the position of an the opcode of an instruction.
    pub end: u16,
    /// Indicates the start of the exception handler.
    /// Must be a valid index into the `code` vec at the position of the opcode of an instruction.
    pub handler: u16,
    /// A nonzero value must be a valid index into the constant pool table at the position of a Class variant
    /// A zero value is used to implement a `finally` block
    pub catch_type: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum VerificationTypeInfo {
    Top,
    Integer,
    Float,
    Double,
    Long,
    Null,
    UninitializedThis,
    Object(u16),
    UninitializedVar { offset: u16 },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FrameType {
    Same {
        offset_delta: u16,
    },
    SameLocals1StackItem {
        offset_delta: u16,
        stack: VerificationTypeInfo,
    }, // Array of VerificationTypes may be incorrect
    SameLocals1StackItemExtended {
        offset_delta: u16,
        stack: VerificationTypeInfo,
    },
    Chop {
        k: u8,
        offset_delta: u16,
    },
    SameExtended {
        offset_delta: u16,
    },
    Append {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
    },
    Full {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    },
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ClassInfo {
    pub inner_class_info_index: u16,
    pub outer_class_info_index: u16,
    pub inner_name_index: u16,
    pub inner_class_access_flags: InnerClassFlags,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct InnerClassFlags {
    is_public: bool,
    is_private: bool,
    is_protected: bool,
    is_static: bool,
    is_final: bool,
    is_interface: bool,
    is_abstract: bool,
    is_synthetic: bool,
    is_annotation: bool,
    is_enum: bool,
}

impl InnerClassFlags {
    pub const PUBLIC: u16 = 0x0001;
    pub const PRIVATE: u16 = 0x0002;
    pub const PROTECTED: u16 = 0x0004;
    pub const STATIC: u16 = 0x0008;
    pub const FINAL: u16 = 0x0010;
    pub const INTERFACE: u16 = 0x0200;
    pub const ABSTRACT: u16 = 0x0400;
    pub const SYNTHETIC: u16 = 0x1000;
    pub const ANNOTATION: u16 = 0x2000;
    pub const ENUM: u16 = 0x4000;

    #[must_use]
    pub const fn from_u16(n: u16) -> InnerClassFlags {
        InnerClassFlags {
            is_public: (n & InnerClassFlags::PUBLIC) != 0,
            is_private: (n & InnerClassFlags::PRIVATE) != 0,
            is_protected: (n & InnerClassFlags::PROTECTED) != 0,
            is_static: (n & InnerClassFlags::STATIC) != 0,
            is_final: (n & InnerClassFlags::FINAL) != 0,
            is_interface: (n & InnerClassFlags::INTERFACE) != 0,
            is_abstract: (n & InnerClassFlags::ABSTRACT) != 0,
            is_synthetic: (n & InnerClassFlags::SYNTHETIC) != 0,
            is_annotation: (n & InnerClassFlags::ANNOTATION) != 0,
            is_enum: (n & InnerClassFlags::ENUM) != 0,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LineNumberTableEntry {
    pub start: u16,
    pub line_number: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LocalVariableTableEntry {
    pub start: u16,
    pub length: u16,
    pub name_index: u16,
    pub descriptor_index: u16,
    pub index: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    pub type_index: u16,
    pub element_value_pairs: Vec<ElementValuePair>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementValuePair {
    pub element_name_index: u16,
    pub element_value: ElementValue,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EnumConstValue {
    pub type_name_index: u16,
    pub const_name_index: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementValue {
    pub tag: u8,
    pub const_value_index: u16,
    pub enum_const_value: EnumConstValue,
    pub class_info_index: u16,
    pub annotation_value: Annotation,
    pub values: Vec<ElementValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumParameters {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BootstrapMethod {
    pub bootstrap_method_ref: u16,
    pub bootstrap_arguments: Vec<u16>,
}
