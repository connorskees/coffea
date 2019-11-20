#[derive(Debug)]
pub struct AttributeInfo {
    pub attribute_name_index: u16,
    pub info: Vec<u8>,
}

#[derive(Debug)]
enum Attribute {
    ConstantValue {
        name_index: u16,
        const_value_index: u16,
    },
    Code {
        name_index: u16,
        code: CodeAttribute,
    },
    StackMapTable {
        name_index: u16,
        entries: Vec<FrameType>,
    },
    Exceptions {
        name_index: u16,
        exception_index_table: Vec<u16>,
    },
    InnerClasses {
        name_index: u16,
        classes: Vec<ClassInfo>,
    },
    EnclosingMethod {
        name_index: u16,
        class_index: u16,
        method_index: u16,
    },
    Synthetic {
        name_index: u16,
    },
    Signature {
        name_index: u16,
        signature_index: u16,
    },
    SourceFile {
        name_index: u16,
        sourcefile_index: u16,
    },
    SourceDebugExtension {
        name_index: u16,
        debug_extensions: Vec<u8>,
    },
    LineNumberTable {
        name_index: u16,
        line_number_table: Vec<LineNumberTableEntry>,
    },
    LocalVariableTable {
        name_index: u16,
        local_variable_table: Vec<LocalVariableTableEntry>,
    },
    LocalVariableTypeTable {
        name_index: u16,
        local_variable_type_table: Vec<LocalVariableTypeTableEntry>,
    },
    Deprecated {
        name_index: u16,
    },
    RuntimeVisibleAnnotations {
        name_index: u16,
    },
    RuntimeInvisibleAnnotations {
        name_index: u16,
    },
    RuntimeVisibleParameterAnnotations {
        name_index: u16,
    },
    RuntimeInvisibleParameterAnnotations {
        name_index: u16,
    },
    AnnotationDefault {
        name_index: u16,
    },
    BootstrapMethods {
        name_index: u16,
    },
    Other {
        name_index: u16,
        info: Vec<u8>,
    },
}

#[derive(Debug)]
struct CodeAttribute {
    max_stack: u16,
    max_locals: u16,
    code: Vec<u8>,
    exception_table: Vec<ExceptionTableEntry>,
    attribute_info: Vec<Attribute>,
}

#[derive(Debug)]
struct ExceptionTableEntry {
    start_pc: u16,
    end_pc: u16,
    handler_pc: u16,
    catch_type: u16,
}

#[derive(Debug)]
enum VerificationTypeInfo {
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
enum FrameType {
    same_frame {},
    same_locals_1_stack_item {
        stack: [VerificationTypeInfo; 1],
    }, // Array of VerificationTypes may be incorrect
    same_locals_1_stack_item_frame_extended {
        offset_delta: u16,
        stack: [VerificationTypeInfo; 1],
    },
    chop_frame {
        offset_delta: u16,
    },
    same_frame_extended {
        offset_delta: u16,
    },
    append_frame {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
    },
    full_frame {
        offset_delta: u16,
        locals: Vec<VerificationTypeInfo>,
        stack: Vec<VerificationTypeInfo>,
    },
}

#[derive(Debug)]
struct ClassInfo {
    inner_class_info_index: u16,
    outer_class_info_index: u16,
    inner_name_index: u16,
    inner_class_access_flags: u16,
}

#[derive(Debug)]
struct LineNumberTableEntry {
    start_pc: u16,
    line_number: u16,
}

#[derive(Debug)]
struct LocalVariableTableEntry {
    start_pc: u16,
    length: u16,
    name_index: u16,
    descriptor_index: u16,
    index: u16,
}

#[derive(Debug)]
struct LocalVariableTypeTableEntry {
    start_pc: u16,
    length: u16,
    name_index: u16,
    signature_index: u16,
    index: u16,
}

#[derive(Debug)]
struct Annotation {
    type_index: u16,
}

struct ElementValuePair {
    element_name_index: u16,
}

struct ElementValue {
    tag: u8,
}

struct BootstrapMethodAttribute {}

// BootstrapMethods_attribute {
//     u2 num_bootstrap_methods;
//     {   u2 bootstrap_method_ref;
//         u2 num_bootstrap_arguments;
//         u2 bootstrap_arguments[num_bootstrap_arguments];
//     } bootstrap_methods[num_bootstrap_methods];
// }
