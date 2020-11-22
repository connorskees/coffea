use crate::{
    attributes::{Attribute, ExceptionTableEntry},
    instructions::Instructions,
};

#[derive(Clone, PartialEq, Eq)]
pub struct Code {
    /// The value of the `max_stack` item gives the
    /// maximum depth of the operand stack of this
    /// method at any point during execution of the method.
    pub max_stack: u16,

    /// The value of the max_locals item gives the
    /// number of local variables in the local variable
    /// array allocated upon invocation of this method,
    /// including the local variables used to pass parameters
    /// to the method on its invocation.
    ///
    /// The greatest local variable index for a value of
    /// type long or double is max_locals - 2. The greatest
    /// local variable index for a value of any other
    /// type is max_locals - 1.
    pub max_locals: u16,

    /// The code array gives the actual bytes of Java
    /// Virtual Machine code that implement the method.
    ///
    /// When the code array is read into memory on a
    /// byte-addressable machine, if the first byte of
    /// the array is aligned on a 4-byte boundary, the
    /// tableswitch and lookupswitch 32-bit offsets will
    /// be 4-byte aligned. (Refer to the descriptions of
    /// those instructions for more information on the
    /// consequences of code array alignment.)
    pub code: Vec<u8>,

    /// Each entry in the exception_table array describes
    /// one exception handler in the code array. The order
    /// of the handlers in the exception_table array is
    /// significant.
    pub exception_table: Vec<ExceptionTableEntry>,

    /// Each value of the attributes table must be an attribute
    /// structure.
    ///
    /// The only attributes allowed as attributes of `Code`
    /// are the `LineNumberTable`, `LocalVariableTable`,
    /// `LocalVariableTypeTable`, and `StackMapTable` attributes.
    pub attribute_info: Vec<Attribute>,
}

impl Code {
    #[must_use]
    pub fn lex(&self) -> Instructions {
        Instructions::new(self.code.iter())
    }
}

impl std::fmt::Debug for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Code {{\n    max_stack: {},\n    max_locals: {},\n    code: {} bytes\n    exception_table: {:?},\n    attribute_info: {:#?}\n}}",
            self.max_stack,
            self.max_locals,
            self.code.len(),
            self.exception_table,
            self.attribute_info
        )
    }
}
