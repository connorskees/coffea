use crate::attributes::{Attribute, ExceptionTableEntry};

#[derive(Clone, PartialEq, Eq)]
pub struct Code {
    pub max_stack: u16,
    pub max_locals: u16,
    pub code: Vec<u8>,
    pub exception_table: Vec<ExceptionTableEntry>,
    pub attribute_info: Vec<Attribute>,
}

impl Code {
    pub fn lex(&self) -> Vec<Instruction> {
        let mut bytes = self.code.iter();
        let mut instructions = Vec::with_capacity(bytes.len());
        while let Some(b) = bytes.next() {
            let mut next = || *bytes.next().unwrap();
            instructions.push(match b {
                0x32 => Instruction::AALoad,
                0x53 => Instruction::AAStore,
                0x01 => Instruction::AConstNull,
                0x19 => Instruction::ALoad(next()),
                0x2a => Instruction::Aload0,
                0x2b => Instruction::Aload1,
                0x2c => Instruction::Aload2,
                0x2d => Instruction::ALoad3,
                0xbd => Instruction::ANewArray(next(), next()),
                0xb0 => Instruction::AReturn,
                0xbe => Instruction::ArrayLength,
                0x3a => Instruction::AStore(next()),
                0x4b => Instruction::AStore0,
                0x4c => Instruction::AStore1,
                0x4d => Instruction::AStore2,
                0x4e => Instruction::AStore3,
                0xbf => Instruction::Athrow,
                0x33 => Instruction::BALoad,
                0x54 => Instruction::BAStore,
                0x10 => Instruction::BiPush(next()),
                0xca => Instruction::Breakpoint,
                0x34 => Instruction::CALoad,
                0x55 => Instruction::CAStore,
                0xc0 => Instruction::Checkcast(next(), next()),
                0x90 => Instruction::D2f,
                0x8e => Instruction::D2i,
                0x8f => Instruction::D2l,
                0x63 => Instruction::DAdd,
                0x31 => Instruction::DALoad,
                0x52 => Instruction::DAStore,
                0x98 => Instruction::Dcmpg,
                0x97 => Instruction::Dcmpl,
                0x0e => Instruction::DConst0,
                0x0f => Instruction::DConst1,
                0x6f => Instruction::Ddiv,
                0x18 => Instruction::Dload(next()),
                0x26 => Instruction::Dload0,
                0x27 => Instruction::Dload1,
                0x28 => Instruction::Dload2,
                0x29 => Instruction::Dload3,
                0x6b => Instruction::Dmul,
                0x77 => Instruction::Dneg,
                0x73 => Instruction::Drem,
                0xaf => Instruction::Dreturn,
                0x39 => Instruction::Dstore(next()),
                0x47 => Instruction::Dstore0,
                0x48 => Instruction::Dstore1,
                0x49 => Instruction::Dstore2,
                0x4a => Instruction::Dstore3,
                0x67 => Instruction::Dsub,
                0x59 => Instruction::Dup,
                0x5a => Instruction::DupX1,
                0x5b => Instruction::DupX2,
                0x5c => Instruction::Dup2,
                0x5d => Instruction::Dup2X1,
                0x5e => Instruction::Dup2X2,
                0x8d => Instruction::F2d,
                0x8b => Instruction::F2i,
                0x8c => Instruction::F2l,
                0x62 => Instruction::Fadd,
                0x30 => Instruction::FALoad,
                0x51 => Instruction::FAStore,
                0x96 => Instruction::Fcmpg,
                0x95 => Instruction::Fcmpl,
                0x0b => Instruction::FConst0,
                0x0c => Instruction::FConst1,
                0x0d => Instruction::FConst2,
                0x6e => Instruction::Fdiv,
                0x17 => Instruction::Fload(next()),
                0x22 => Instruction::Fload0,
                0x23 => Instruction::Fload1,
                0x24 => Instruction::Fload2,
                0x25 => Instruction::Fload3,
                0x6a => Instruction::Fmul,
                0x76 => Instruction::Fneg,
                0x72 => Instruction::Frem,
                0xae => Instruction::Freturn,
                0x38 => Instruction::Fstore(next()),
                0x43 => Instruction::Fstore0,
                0x44 => Instruction::Fstore1,
                0x45 => Instruction::Fstore2,
                0x46 => Instruction::Fstore3,
                0x66 => Instruction::Fsub,
                0xb4 => Instruction::GetField(u16::from_be_bytes([next(), next()])),
                0xb2 => Instruction::GetStatic(u16::from_be_bytes([next(), next()])),
                0xa7 => Instruction::Goto(next(), next()),
                0xc8 => Instruction::GotoW(next(), next(), next(), next()),
                0x91 => Instruction::I2b,
                0x92 => Instruction::I2c,
                0x87 => Instruction::I2d,
                0x86 => Instruction::I2f,
                0x85 => Instruction::I2l,
                0x93 => Instruction::I2s,
                0x60 => Instruction::Iadd,
                0x2e => Instruction::IALoad,
                0x7e => Instruction::Iand,
                0x4f => Instruction::IAStore,
                0x02 => Instruction::IConstM1,
                0x03 => Instruction::IConst0,
                0x04 => Instruction::IConst1,
                0x05 => Instruction::IConst2,
                0x06 => Instruction::IConst3,
                0x07 => Instruction::IConst4,
                0x08 => Instruction::IConst5,
                0x6c => Instruction::Idiv,
                0xa5 => Instruction::IfAcmpeq(next(), next()),
                0xa6 => Instruction::IfAcmpne(next(), next()),
                0x9f => Instruction::IfIcmpeq(next(), next()),
                0xa2 => Instruction::IfIcmpge(next(), next()),
                0xa3 => Instruction::IfIcmpgt(next(), next()),
                0xa4 => Instruction::IfIcmple(next(), next()),
                0xa1 => Instruction::IfIcmplt(next(), next()),
                0xa0 => Instruction::IfIcmpne(next(), next()),
                0x99 => Instruction::Ifeq(next(), next()),
                0x9c => Instruction::Ifge(next(), next()),
                0x9d => Instruction::Ifgt(next(), next()),
                0x9e => Instruction::Ifle(next(), next()),
                0x9b => Instruction::Iflt(next(), next()),
                0x9a => Instruction::Ifne(next(), next()),
                0xc7 => Instruction::Ifnonnull(next(), next()),
                0xc6 => Instruction::Ifnull(next(), next()),
                0x84 => Instruction::Iinc(next(), next()),
                0x15 => Instruction::ILoad(next()),
                0x1a => Instruction::ILoad0,
                0x1b => Instruction::ILoad1,
                0x1c => Instruction::ILoad2,
                0x1d => Instruction::ILoad3,
                0xfe => Instruction::Impdep1,
                0xff => Instruction::Impdep2,
                0x68 => Instruction::Imul,
                0x74 => Instruction::Ineg,
                0xc1 => Instruction::InstanceOf(next(), next()),
                0xba => Instruction::InvokeDynamic(next(), next(), next(), next()),
                0xb9 => Instruction::InvokeInterface(next(), next(), next(), next()),
                0xb7 => Instruction::InvokeSpecial(next(), next()),
                0xb8 => Instruction::InvokeStatic(u16::from_be_bytes([next(), next()])),
                0xb6 => Instruction::InvokeVirtual(u16::from_be_bytes([next(), next()])),
                0x80 => Instruction::Ior,
                0x70 => Instruction::Irem,
                0xac => Instruction::Ireturn,
                0x78 => Instruction::Ishl,
                0x7a => Instruction::Ishr,
                0x36 => Instruction::IStore(next()),
                0x3b => Instruction::IStore0,
                0x3c => Instruction::IStore1,
                0x3d => Instruction::IStore2,
                0x3e => Instruction::IStore3,
                0x64 => Instruction::Isub,
                0x7c => Instruction::Iushr,
                0x82 => Instruction::Ixor,
                0xa8 => Instruction::Jsr(next(), next()),
                0xc9 => Instruction::JsrW(next(), next(), next(), next()),
                0x8a => Instruction::L2d,
                0x89 => Instruction::L2f,
                0x88 => Instruction::L2i,
                0x61 => Instruction::Ladd,
                0x2f => Instruction::Laload,
                0x7f => Instruction::Land,
                0x50 => Instruction::Lastore,
                0x94 => Instruction::Lcmp,
                0x09 => Instruction::LConst0,
                0x0a => Instruction::LConst1,
                0x12 => Instruction::Ldc(next()),
                0x13 => Instruction::LdcW(next(), next()),
                0x14 => Instruction::Ldc2W(next(), next()),
                0x6d => Instruction::Ldiv,
                0x16 => Instruction::Lload(next()),
                0x1e => Instruction::LLoad0,
                0x1f => Instruction::LLoad1,
                0x20 => Instruction::LLoad2,
                0x21 => Instruction::LLoad3,
                0x69 => Instruction::Lmul,
                0x75 => Instruction::Lneg,
                0xab => unimplemented!("instruction `LookupSwitch` not yet implemented"), //Instruction::Lookupswitch,
                0x81 => Instruction::Lor,
                0x71 => Instruction::Lrem,
                0xad => Instruction::Lreturn,
                0x79 => Instruction::Lshl,
                0x7b => Instruction::Lshr,
                0x37 => Instruction::Lstore(next()),
                0x3f => Instruction::LStore0,
                0x40 => Instruction::LStore1,
                0x41 => Instruction::LStore2,
                0x42 => Instruction::LStore3,
                0x65 => Instruction::Lsub,
                0x7d => Instruction::Lushr,
                0x83 => Instruction::Lxor,
                0xc2 => Instruction::MonitorEnter,
                0xc3 => Instruction::MonitorExit,
                0xc5 => Instruction::MultiANewArray(next(), next(), next()),
                0xbb => Instruction::New(next(), next()),
                0xbc => Instruction::NewArray(next()),
                0x00 => Instruction::Nop,
                0x57 => Instruction::Pop,
                0x58 => Instruction::Pop2,
                0xb5 => Instruction::PutField(next(), next()),
                0xb3 => Instruction::PutStatic(next(), next()),
                0xa9 => Instruction::Ret(next()),
                0xb1 => Instruction::Return,
                0x35 => Instruction::SAload,
                0x56 => Instruction::SAstore,
                0x11 => Instruction::SIPush(u16::from_be_bytes([next(), next()])),
                0x5f => Instruction::Swap,
                0xaa => unimplemented!("instruction `TableSwitch` not yet implemented"), //Instruction::TableSwitch,
                0xc4 => {
                    let n  = next();
                    match n {
                        0x84 => Instruction::Wide5(n, next(), next(), next(), next()),
                        0x15..=0x19 | 0x36..=0x39 | 0x89 => Instruction::Wide3(n, next(), next()),
                        _ => unimplemented!("invalid opcode in `Instruction::Wide3`; error handling not yet implemented"),
                    }
                }
                0xcb..=0xfd => Instruction::NoName,
            })
        }
        instructions
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

#[repr(u16)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Instruction {
    ///	load onto the stack a reference from an array
    AALoad,
    ///	store a reference in an array
    AAStore,
    ///	push a null reference onto the stack
    AConstNull,
    /// load a reference onto the stack from a local variable #index
    ALoad(u8),
    ///	load a reference onto the stack from local variable 0
    Aload0,
    ///	load a reference onto the stack from local variable 1
    Aload1,
    ///	load a reference onto the stack from local variable 2
    Aload2,
    ///	load a reference onto the stack from local variable 3
    ALoad3,
    /// create a new array of references of length count and component type identified by the class reference index (indexbyte1 << 8 + indexbyte2) in the constant pool
    ANewArray(u8, u8),
    /// return a reference from a method
    AReturn,
    ///	get the length of an array
    ArrayLength,
    /// store a reference into a local variable #index
    AStore(u8),
    ///	store a reference into local variable 0
    AStore0,
    ///	store a reference into local variable 1
    AStore1,
    ///	store a reference into local variable 2
    AStore2,
    ///	store a reference into local variable 3
    AStore3,
    ///	throws an error or exception (notice that the rest of the stack is cleared, leaving only a reference to the Throwable)
    Athrow,
    /// load a byte or Boolean value from an array
    BALoad,
    /// store a byte or Boolean value into an array
    BAStore,
    /// push a byte onto the stack as an integer value
    BiPush(u8),
    /// reserved for breakpoints in Java debuggers; should not appear in any class file
    Breakpoint,
    /// load a char from an array
    CALoad,
    /// store a char into an array
    CAStore,
    /// checks whether an objectref is of a certain type, the class reference of which is in the constant pool at index (indexbyte1 << 8 + indexbyte2)
    Checkcast(u8, u8),
    /// convert a double to a float
    D2f,
    /// convert a double to an int
    D2i,
    /// convert a double to a long
    D2l,
    /// add two doubles
    DAdd,
    /// load a double from an array
    DALoad,
    /// store a double into an array
    DAStore,
    /// compare two doubles
    Dcmpg,
    /// compare two doubles
    Dcmpl,
    /// push the constant 0.0 (a double) onto the stack
    DConst0,
    /// push the constant 1.0 (a double) onto the stack
    DConst1,
    /// divide two doubles
    Ddiv,
    /// load a double value from a local variable #index
    Dload(u8),
    /// load a double from local variable 0
    Dload0,
    /// load a double from local variable 1
    Dload1,
    /// load a double from local variable 2
    Dload2,
    /// load a double from local variable 3
    Dload3,
    /// multiply two doubles
    Dmul,
    /// negate a double
    Dneg,
    /// get the remainder from a division between two doubles
    Drem,
    /// return a double from a method
    Dreturn,
    /// store a double value into a local variable #index
    Dstore(u8),
    /// store a double into local variable 0
    Dstore0,
    /// store a double into local variable 1
    Dstore1,
    /// store a double into local variable 2
    Dstore2,
    /// store a double into local variable 3
    Dstore3,
    /// subtract a double from another
    Dsub,
    /// duplicate the value on top of the stack
    Dup,
    /// insert a copy of the top value into the stack two values from the top. value1 and value2 must not be of the type double or long.
    DupX1,
    /// insert a copy of the top value into the stack two (if value2 is double or long it takes up the entry of value3, too) or three values (if value2 is neither double nor long) from the top
    DupX2,
    /// duplicate top two stack words (two values, if value1 is not double nor long; a single value, if value1 is double or long)
    Dup2,
    /// duplicate two words and insert beneath third word (see explanation above)
    Dup2X1,
    /// duplicate two words and insert beneath fourth word
    Dup2X2,
    /// convert a float to a double
    F2d,
    /// convert a float to an int
    F2i,
    /// convert a float to a long
    F2l,
    /// add two floats
    Fadd,
    /// load a float from an array
    FALoad,
    /// store a float in an array
    FAStore,
    /// compare two floats
    Fcmpg,
    /// compare two floats
    Fcmpl,
    /// push 0.0f on the stack
    FConst0,
    /// push 1.0f on the stack
    FConst1,
    /// push 2.0f on the stack
    FConst2,
    /// divide two floats
    Fdiv,
    /// load a float value from a local variable #index
    Fload(u8),
    /// load a float value from local variable 0
    Fload0,
    /// load a float value from local variable 1
    Fload1,
    /// load a float value from local variable 2
    Fload2,
    /// load a float value from local variable 3
    Fload3,
    /// multiply two floats
    Fmul,
    /// negate a float
    Fneg,
    /// get the remainder from a division between two floats
    Frem,
    /// return a float
    Freturn,
    /// store a float value into a local variable #index
    Fstore(u8),
    /// store a float value into local variable 0
    Fstore0,
    /// store a float value into local variable 1
    Fstore1,
    /// store a float value into local variable 2
    Fstore2,
    /// store a float value into local variable 3
    Fstore3,
    /// subtract two floats
    Fsub,
    /// get a field value of an object objectref, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)
    GetField(u16),
    /// get a static field value of a class, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)
    GetStatic(u16),
    /// goes to another instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Goto(u8, u8),
    /// goes to another instruction at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4)
    GotoW(u8, u8, u8, u8),
    /// convert an int into a byte
    I2b,
    /// convert an int into a character
    I2c,
    /// convert an int into a double
    I2d,
    /// convert an int into a float
    I2f,
    /// convert an int into a long
    I2l,
    /// convert an int into a short
    I2s,
    /// add two ints
    Iadd,
    /// load an int from an array
    IALoad,
    /// perform a bitwise AND on two integers
    Iand,
    /// store an int into an array
    IAStore,
    /// load the int value −1 onto the stack
    IConstM1,
    /// load the int value 0 onto the stack
    IConst0,
    /// load the int value 1 onto the stack
    IConst1,
    /// load the int value 2 onto the stack
    IConst2,
    /// load the int value 3 onto the stack
    IConst3,
    /// load the int value 4 onto the stack
    IConst4,
    /// load the int value 5 onto the stack
    IConst5,
    /// divide two integers
    Idiv,
    /// if references are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfAcmpeq(u8, u8),
    /// if references are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfAcmpne(u8, u8),
    /// if ints are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpeq(u8, u8),
    /// if value1 is greater than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpge(u8, u8),
    /// if value1 is greater than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpgt(u8, u8),
    /// if value1 is less than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmple(u8, u8),
    /// if value1 is less than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmplt(u8, u8),
    /// if ints are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpne(u8, u8),
    /// if value is 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifeq(u8, u8),
    /// if value is greater than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifge(u8, u8),
    /// if value is greater than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifgt(u8, u8),
    /// if value is less than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifle(u8, u8),
    /// if value is less than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Iflt(u8, u8),
    /// if value is not 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifne(u8, u8),
    /// if value is not null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifnonnull(u8, u8),
    /// if value is null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifnull(u8, u8),
    /// increment local variable #index by signed byte const
    Iinc(u8, u8),
    /// load an int value from a local variable #index
    ILoad(u8),
    /// load an int value from local variable 0
    ILoad0,
    /// load an int value from local variable 1
    ILoad1,
    /// load an int value from local variable 2
    ILoad2,
    /// load an int value from local variable 3
    ILoad3,
    /// reserved for implementation-dependent operations within debuggers; should not appear in any class file
    Impdep1,
    /// reserved for implementation-dependent operations within debuggers; should not appear in any class file
    Impdep2,
    /// multiply two integers
    Imul,
    /// negate int
    Ineg,
    /// determines if an object objectref is of a given type, identified by class reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InstanceOf(u8, u8),
    /// invokes a dynamic method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeDynamic(u8, u8, u8, u8),
    /// invokes an interface method on object objectref and puts the result on the stack (might be void); the interface method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeInterface(u8, u8, u8, u8),
    /// invoke instance method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeSpecial(u8, u8),
    /// invoke a static method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeStatic(u16),
    /// invoke virtual method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeVirtual(u16),
    /// bitwise int OR
    Ior,
    /// logical int remainder
    Irem,
    /// return an integer from a method
    Ireturn,
    /// int shift left
    Ishl,
    /// int arithmetic shift right
    Ishr,
    /// store int value into variable #index
    IStore(u8),
    /// store int value into variable 0
    IStore0,
    /// store int value into variable 1
    IStore1,
    /// store int value into variable 2
    IStore2,
    /// store int value into variable 3
    IStore3,
    /// int subtract
    Isub,
    /// int logical shift right
    Iushr,
    /// int xor
    Ixor,
    /// jump to subroutine at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2) and place the return address on the stack
    Jsr(u8, u8),
    /// jump to subroutine at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4) and place the return address on the stack
    JsrW(u8, u8, u8, u8),
    /// convert a long to a double
    L2d,
    /// convert a long to a float
    L2f,
    /// convert a long to a int
    L2i,
    /// add two longs
    Ladd,
    /// load a long from an array
    Laload,
    /// bitwise AND of two longs
    Land,
    /// store a long to an array
    Lastore,
    /// push 0 if the two longs are the same, 1 if value1 is greater than value2, -1 otherwise
    Lcmp,
    /// push 0L (the number zero with type long) onto the stack
    LConst0,
    /// push 1L (the number one with type long) onto the stack
    LConst1,
    /// push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, java.lang.invoke.MethodHandle, or a dynamically-computed constant) onto the stack
    Ldc(u8),
    /// push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, java.lang.invoke.MethodHandle, or a dynamically-computed constant) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)
    LdcW(u8, u8),
    /// push a constant #index from a constant pool (double, long, or a dynamically-computed constant) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)
    Ldc2W(u8, u8),
    /// divide two longs
    Ldiv,
    /// load a long value from a local variable #index
    Lload(u8),
    /// load a long value from a local variable 0
    LLoad0,
    /// load a long value from a local variable 1
    LLoad1,
    /// load a long value from a local variable 2
    LLoad2,
    /// load a long value from a local variable 3
    LLoad3,
    /// multiply two longs
    Lmul,
    /// negate a long
    Lneg,
    /// 8+: <0–3 bytes padding>, defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, npairs1, npairs2, npairs3, npairs4, match-offset pairs...	key →	a target address is looked up from a table using a key and execution continues from the instruction at that address
    Lookupswitch,
    /// bitwise OR of two longs
    Lor,
    /// remainder of division of two longs
    Lrem,
    /// return a long value
    Lreturn,
    /// bitwise shift left of a long value1 by int value2 positions
    Lshl,
    /// bitwise shift right of a long value1 by int value2 positions
    Lshr,
    /// store a long value in a local variable #index
    Lstore(u8),
    /// store a long value in a local variable 0
    LStore0,
    /// store a long value in a local variable 1
    LStore1,
    /// store a long value in a local variable 2
    LStore2,
    /// store a long value in a local variable 3
    LStore3,
    /// subtract two longs
    Lsub,
    /// bitwise shift right of a long value1 by int value2 positions, unsigned
    Lushr,
    /// bitwise XOR of two longs
    Lxor,
    /// enter monitor for object ("grab the lock" – start of synchronized() section)
    MonitorEnter,
    /// exit monitor for object ("release the lock" – end of synchronized() section)
    MonitorExit,
    /// create a new array of dimensions dimensions of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2); the sizes of each dimension is identified by count1, [count2, etc.]
    MultiANewArray(u8, u8, u8),
    /// create new object of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2)
    New(u8, u8),
    /// create new array with count elements of primitive type identified by atype
    NewArray(u8),
    /// perform no operation
    Nop,
    /// discard the top value on the stack
    Pop,
    /// discard the top two values on the stack (or one value, if it is a double or long)
    Pop2,
    /// set field to value in an object objectref, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    PutField(u8, u8),
    /// set static field to value in a class, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    PutStatic(u8, u8),
    /// continue execution from address taken from a local variable #index (the asymmetry with jsr is intentional)
    Ret(u8),
    /// return void from method
    Return,
    /// load short from array
    SAload,
    /// store short to array
    SAstore,
    /// push a short onto the stack as an integer value
    SIPush(u16),
    /// swaps two top words on the stack (note that value1 and value2 must not be double or long)
    Swap,
    /// 16+: [0–3 bytes padding], defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, lowbyte1, lowbyte2, lowbyte3, lowbyte4, highbyte1, highbyte2, highbyte3, highbyte4, jump offsets...	index →	continue execution from an address in the table at offset index
    TableSwitch,
    ///  execute opcode, where opcode is either iload, fload, aload, lload, dload, istore, fstore, astore, lstore, dstore, or ret, but assume the index is 16 bit
    Wide3(u8, u8, u8),
    /// execute iinc, where the index is 16 bits and the constant to increment by is a signed 16 bit short
    Wide5(u8, u8, u8, u8, u8),
    NoName, //= 0xcb..=0xfd
}

impl Instruction {
    pub(crate) fn len(&self) -> u8 {
        match self {
            Instruction::AALoad => 1,
            Instruction::AAStore => 1,
            Instruction::AConstNull => 1,
            Instruction::ALoad(_) => 2,
            Instruction::Aload0 => 1,
            Instruction::Aload1 => 1,
            Instruction::Aload2 => 1,
            Instruction::ALoad3 => 1,
            Instruction::ANewArray(_, _) => 3,
            Instruction::AReturn => 1,
            Instruction::ArrayLength => 1,
            Instruction::AStore(_) => 2,
            Instruction::AStore0 => 1,
            Instruction::AStore1 => 1,
            Instruction::AStore2 => 1,
            Instruction::AStore3 => 1,
            Instruction::Athrow => 1,
            Instruction::BALoad => 1,
            Instruction::BAStore => 1,
            Instruction::BiPush(_) => 2,
            Instruction::Breakpoint => 1,
            Instruction::CALoad => 1,
            Instruction::CAStore => 1,
            Instruction::Checkcast(_, _) => 3,
            Instruction::D2f => 1,
            Instruction::D2i => 1,
            Instruction::D2l => 1,
            Instruction::DAdd => 1,
            Instruction::DALoad => 1,
            Instruction::DAStore => 1,
            Instruction::Dcmpg => 1,
            Instruction::Dcmpl => 1,
            Instruction::DConst0 => 1,
            Instruction::DConst1 => 1,
            Instruction::Ddiv => 1,
            Instruction::Dload(_) => 2,
            Instruction::Dload0 => 1,
            Instruction::Dload1 => 1,
            Instruction::Dload2 => 1,
            Instruction::Dload3 => 1,
            Instruction::Dmul => 1,
            Instruction::Dneg => 1,
            Instruction::Drem => 1,
            Instruction::Dreturn => 1,
            Instruction::Dstore(_) => 2,
            Instruction::Dstore0 => 1,
            Instruction::Dstore1 => 1,
            Instruction::Dstore2 => 1,
            Instruction::Dstore3 => 1,
            Instruction::Dsub => 1,
            Instruction::Dup => 1,
            Instruction::DupX1 => 1,
            Instruction::DupX2 => 1,
            Instruction::Dup2 => 1,
            Instruction::Dup2X1 => 1,
            Instruction::Dup2X2 => 1,
            Instruction::F2d => 1,
            Instruction::F2i => 1,
            Instruction::F2l => 1,
            Instruction::Fadd => 1,
            Instruction::FALoad => 1,
            Instruction::FAStore => 1,
            Instruction::Fcmpg => 1,
            Instruction::Fcmpl => 1,
            Instruction::FConst0 => 1,
            Instruction::FConst1 => 1,
            Instruction::FConst2 => 1,
            Instruction::Fdiv => 1,
            Instruction::Fload(_) => 2,
            Instruction::Fload0 => 1,
            Instruction::Fload1 => 1,
            Instruction::Fload2 => 1,
            Instruction::Fload3 => 1,
            Instruction::Fmul => 1,
            Instruction::Fneg => 1,
            Instruction::Frem => 1,
            Instruction::Freturn => 1,
            Instruction::Fstore(_) => 2,
            Instruction::Fstore0 => 1,
            Instruction::Fstore1 => 1,
            Instruction::Fstore2 => 1,
            Instruction::Fstore3 => 1,
            Instruction::Fsub => 1,
            Instruction::GetField(_) => 2,
            Instruction::GetStatic(_) => 2,
            Instruction::Goto(_, _) => 3,
            Instruction::GotoW(_, _, _, _) => 5,
            Instruction::I2b => 1,
            Instruction::I2c => 1,
            Instruction::I2d => 1,
            Instruction::I2f => 1,
            Instruction::I2l => 1,
            Instruction::I2s => 1,
            Instruction::Iadd => 1,
            Instruction::IALoad => 1,
            Instruction::Iand => 1,
            Instruction::IAStore => 1,
            Instruction::IConstM1 => 1,
            Instruction::IConst0 => 1,
            Instruction::IConst1 => 1,
            Instruction::IConst2 => 1,
            Instruction::IConst3 => 1,
            Instruction::IConst4 => 1,
            Instruction::IConst5 => 1,
            Instruction::Idiv => 1,
            Instruction::IfAcmpeq(_, _) => 3,
            Instruction::IfAcmpne(_, _) => 3,
            Instruction::IfIcmpeq(_, _) => 3,
            Instruction::IfIcmpge(_, _) => 3,
            Instruction::IfIcmpgt(_, _) => 3,
            Instruction::IfIcmple(_, _) => 3,
            Instruction::IfIcmplt(_, _) => 3,
            Instruction::IfIcmpne(_, _) => 3,
            Instruction::Ifeq(_, _) => 3,
            Instruction::Ifge(_, _) => 3,
            Instruction::Ifgt(_, _) => 3,
            Instruction::Ifle(_, _) => 3,
            Instruction::Iflt(_, _) => 3,
            Instruction::Ifne(_, _) => 3,
            Instruction::Ifnonnull(_, _) => 3,
            Instruction::Ifnull(_, _) => 3,
            Instruction::Iinc(_, _) => 3,
            Instruction::ILoad(_) => 2,
            Instruction::ILoad0 => 1,
            Instruction::ILoad1 => 1,
            Instruction::ILoad2 => 1,
            Instruction::ILoad3 => 1,
            Instruction::Impdep1 => 1,
            Instruction::Impdep2 => 1,
            Instruction::Imul => 1,
            Instruction::Ineg => 1,
            Instruction::InstanceOf(_, _) => 3,
            Instruction::InvokeDynamic(_, _, _, _) => 5,
            Instruction::InvokeInterface(_, _, _, _) => 5,
            Instruction::InvokeSpecial(_, _) => 3,
            Instruction::InvokeStatic(_) => 2,
            Instruction::InvokeVirtual(_) => 2,
            Instruction::Ior => 1,
            Instruction::Irem => 1,
            Instruction::Ireturn => 1,
            Instruction::Ishl => 1,
            Instruction::Ishr => 1,
            Instruction::IStore(_) => 2,
            Instruction::IStore0 => 1,
            Instruction::IStore1 => 1,
            Instruction::IStore2 => 1,
            Instruction::IStore3 => 1,
            Instruction::Isub => 1,
            Instruction::Iushr => 1,
            Instruction::Ixor => 1,
            Instruction::Jsr(_, _) => 3,
            Instruction::JsrW(_, _, _, _) => 5,
            Instruction::L2d => 1,
            Instruction::L2f => 1,
            Instruction::L2i => 1,
            Instruction::Ladd => 1,
            Instruction::Laload => 1,
            Instruction::Land => 1,
            Instruction::Lastore => 1,
            Instruction::Lcmp => 1,
            Instruction::LConst0 => 1,
            Instruction::LConst1 => 1,
            Instruction::Ldc(_) => 2,
            Instruction::LdcW(_, _) => 3,
            Instruction::Ldc2W(_, _) => 3,
            Instruction::Ldiv => 1,
            Instruction::Lload(_) => 2,
            Instruction::LLoad0 => 1,
            Instruction::LLoad1 => 1,
            Instruction::LLoad2 => 1,
            Instruction::LLoad3 => 1,
            Instruction::Lmul => 1,
            Instruction::Lneg => 1,
            Instruction::Lookupswitch => unimplemented!("instruction `LookupSwitch` not yet implemented"), //Instruction::Lookupswitch,
            Instruction::Lor => 1,
            Instruction::Lrem => 1,
            Instruction::Lreturn => 1,
            Instruction::Lshl => 1,
            Instruction::Lshr => 1,
            Instruction::Lstore(_) => 2,
            Instruction::LStore0 => 1,
            Instruction::LStore1 => 1,
            Instruction::LStore2 => 1,
            Instruction::LStore3 => 1,
            Instruction::Lsub => 1,
            Instruction::Lushr => 1,
            Instruction::Lxor => 1,
            Instruction::MonitorEnter => 1,
            Instruction::MonitorExit => 1,
            Instruction::MultiANewArray(_, _, _) => 4,
            Instruction::New(_, _) => 3,
            Instruction::NewArray(_) => 2,
            Instruction::Nop => 1,
            Instruction::Pop => 1,
            Instruction::Pop2 => 1,
            Instruction::PutField(_, _) => 3,
            Instruction::PutStatic(_, _) => 3,
            Instruction::Ret(_) => 2,
            Instruction::Return => 1,
            Instruction::SAload => 1,
            Instruction::SAstore => 1,
            Instruction::SIPush(_) => 2,
            Instruction::Swap => 1,
            Instruction::TableSwitch => unimplemented!("instruction `TableSwitch` not yet implemented"),
            Instruction::Wide3(_, _, _) => 4,
            Instruction::Wide5(_, _, _, _, _) => 6,
            Instruction::NoName => 1,
        }
    }

}