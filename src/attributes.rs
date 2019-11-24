#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Attribute {
    ConstantValue { const_value_index: u16 },
    Code(Code),
    StackMapTable(Vec<FrameType>),
    Exceptions { exception_index_table: Vec<u16> },
    InnerClasses(Vec<ClassInfo>),
    EnclosingMethod { class_index: u16, method_index: u16 },
    Synthetic,
    Signature(u16),
    SourceFile(u16),
    SourceDebugExtension { debug_extensions: Vec<u8> },
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
    Other { info: Vec<u8> },
}

#[derive(Clone, PartialEq, Eq)]
pub struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<u8>,
    pub exception_table: Vec<ExceptionTableEntry>,
    pub attribute_info: Vec<Attribute>,
}

impl std::fmt::Debug for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Code {{\n    max_stack: {},\n    max_locals: {},\n    code: {} bytes\n}}",
            self.max_stack,
            self.max_locals,
            self.code.len()
        )
    }
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
    pub inner_class_access_flags: u16,
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
    type_index: u16,
    element_value_pairs: Vec<ElementValuePair>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementValuePair {
    element_name_index: u16,
    element_value: ElementValue,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EnumConstValue {
    type_name_index: u16,
    const_name_index: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementValue {
    tag: u8,
    enum_const_value: EnumConstValue,
    clas_info_index: u16,
    annotation_value: Annotation,
    values: Vec<ElementValuePair>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumParameters {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BootstrapMethod {
    pub bootstrap_method_ref: u16,
    pub bootstrap_arguments: Vec<u16>,
}
