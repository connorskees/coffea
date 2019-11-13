
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
    StackMapTable,
    Exceptions,
    InnerClasses,
    EnclosingMethod,
    Synthetic,
    Signature,
    SourceFile,
    SourceDebugExtension,
    LineNumberTable,
    LocalVariableTable,
    LocalVariableTypeTable,
    Deprecated,
    RuntimeVisibleAnnotations,
    RuntimeInvisibleAnnotations,
    RuntimeVisibleParameterAnnotations,
    RuntimeInvisibleParameterAnnotations,
    AnnotationDefault,
    BootstrapMethods,
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