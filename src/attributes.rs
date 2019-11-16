
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
    StackMapTable{
        name_index: u16,
    },
    Exceptions{
        name_index: u16,
    },
    InnerClasses{
        name_index: u16,
    },
    EnclosingMethod{
        name_index: u16,
    },
    Synthetic{
        name_index: u16,
    },
    Signature{
        name_index: u16,
    },
    SourceFile{
        name_index: u16,
    },
    SourceDebugExtension{
        name_index: u16,
    },
    LineNumberTable{
        name_index: u16,
    },
    LocalVariableTable{
        name_index: u16,
    },
    LocalVariableTypeTable{
        name_index: u16,
    },
    Deprecated{
        name_index: u16,
    },
    RuntimeVisibleAnnotations{
        name_index: u16,
    },
    RuntimeInvisibleAnnotations{
        name_index: u16,
    },
    RuntimeVisibleParameterAnnotations{
        name_index: u16,
    },
    RuntimeInvisibleParameterAnnotations{
        name_index: u16,
    },
    AnnotationDefault{
        name_index: u16,
    },
    BootstrapMethods{
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

struct BootstrapMethodAttribute {

}

// BootstrapMethods_attribute {
//     u2 num_bootstrap_methods;
//     {   u2 bootstrap_method_ref;
//         u2 num_bootstrap_arguments;
//         u2 bootstrap_arguments[num_bootstrap_arguments];
//     } bootstrap_methods[num_bootstrap_methods];
// }
