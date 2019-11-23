pub struct AttributeInfo {
    pub attribute_name_index: u16,
    pub info: Vec<u8>,
}

impl std::fmt::Debug for AttributeInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "AttributeInfo {{ name_index: {}, info: {} bytes }}",
            self.attribute_name_index,
            self.info.len()
        )
    }
}

#[derive(Debug)]
pub enum Attribute {
    ConstantValue {
        const_value_index: u16,
    },
    Code {
        code: CodeAttribute,
    },
    StackMapTable {
        entries: Vec<FrameType>,
    },
    Exceptions {
        exception_index_table: Vec<u16>,
    },
    InnerClasses {
        classes: Vec<ClassInfo>,
    },
    EnclosingMethod {
        class_index: u16,
        method_index: u16,
    },
    Synthetic,
    Signature {
        signature_index: u16,
    },
    SourceFile {
        sourcefile_index: u16,
    },
    SourceDebugExtension {
        debug_extensions: Vec<u8>,
    },
    LineNumberTable {
        line_number_table: Vec<LineNumberTableEntry>,
    },
    LocalVariableTable {
        local_variable_table: Vec<LocalVariableTableEntry>,
    },
    LocalVariableTypeTable {
        local_variable_type_table: Vec<LocalVariableTypeTableEntry>,
    },
    Deprecated,
    RuntimeVisibleAnnotations,
    RuntimeInvisibleAnnotations,
    RuntimeVisibleParameterAnnotations,
    RuntimeInvisibleParameterAnnotations,
    AnnotationDefault,
    BootstrapMethods,
    Other {
        info: Vec<u8>,
    },
}

impl Attribute {
    pub fn from_bytes(b: &[u8]) -> Attribute {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct CodeAttribute {
    max_stack: u16,
    max_locals: u16,
    code: Vec<u8>,
    exception_table: Vec<ExceptionTableEntry>,
    attribute_info: Vec<Attribute>,
}

#[derive(Debug)]
pub struct ExceptionTableEntry {
    start_pc: u16,
    end_pc: u16,
    handler_pc: u16,
    catch_type: u16,
}

#[derive(Debug)]
pub enum VerificationTypeInfo {
    Top {},
    Integer {},
    Float {},
    Double {},
    Long {},
    Null {},
    UninitializedThis {},
    Object { cpool_index: u16 },
    Uninitialized { offset: u16 },
}

#[derive(Debug)]
pub enum FrameType {
    SameFrame,
    SameLocals1StackItem {
        stack: [VerificationTypeInfo; 1],
    }, // Array of VerificationTypes may be incorrect
    SameLocals1StackItemFrameExtended {
        offset_delta: u16,
        stack: [VerificationTypeInfo; 1],
    },
    ChopFrame {
        offset_delta: u16,
    },
    SameFrameExtended {
        offset_delta: u16,
    },
    AppendFrame {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
    },
    FullFrame {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    },
}

#[derive(Debug)]
pub struct ClassInfo {
    inner_class_info_index: u16,
    outer_class_info_index: u16,
    inner_name_index: u16,
    inner_class_access_flags: u16,
}

#[derive(Debug)]
pub struct LineNumberTableEntry {
    start_pc: u16,
    line_number: u16,
}

#[derive(Debug)]
pub struct LocalVariableTableEntry {
    start_pc: u16,
    length: u16,
    name_index: u16,
    descriptor_index: u16,
    index: u16,
}

#[derive(Debug)]
pub struct LocalVariableTypeTableEntry {
    start_pc: u16,
    length: u16,
    name_index: u16,
    signature_index: u16,
    index: u16,
}

#[derive(Debug)]
pub struct Annotation {
    type_index: u16,
}

#[derive(Debug)]
pub struct ElementValuePair {
    element_name_index: u16,
}

#[derive(Debug)]
pub struct ElementValue {
    tag: u8,
}

#[derive(Debug)]
struct BootstrapMethodAttribute {}

// BootstrapMethods_attribute {
//     u2 num_bootstrap_methods;
//     {   u2 bootstrap_method_ref;
//         u2 num_bootstrap_arguments;
//         u2 bootstrap_arguments[num_bootstrap_arguments];
//     } bootstrap_methods[num_bootstrap_methods];
// }
