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
    pub fn parse(&self) -> Vec<Instruction> {
        unimplemented!()
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
    AALoad, //= 0x32,
    ///	store a reference in an array
    AAStore, //= 0x53,
    ///	push a null reference onto the stack
    AConstNull, //= 0x01,
    /// load a reference onto the stack from a local variable #index
    ALoad(u8), //= 0x19,
    ///	load a reference onto the stack from local variable 0
    Aload0, //= 0x2a,
    ///	load a reference onto the stack from local variable 1
    Aload1, //= 0x2b,
    ///	load a reference onto the stack from local variable 2
    Aload2, //= 0x2c,
    ///	load a reference onto the stack from local variable 3
    ALoad3, //= 0x2d,
    /// create a new array of references of length count and component type identified by the class reference index (indexbyte1 << 8 + indexbyte2) in the constant pool
    ANewArray(u8, u8), //= 0xbd,
    /// return a reference from a method
    AReturn, //= 0xb0,
    ///	get the length of an array
    ArrayLength, //= 0xbe,
    /// store a reference into a local variable #index
    AStore(u8), //= 0x3a,
    ///	store a reference into local variable 0
    AStore0, //= 0x4b,
    ///	store a reference into local variable 1
    AStore1, //= 0x4c,
    ///	store a reference into local variable 2
    AStore2, //= 0x4d,
    ///	store a reference into local variable 3
    AStore3, //= 0x4e,
    ///	throws an error or exception (notice that the rest of the stack is cleared, leaving only a reference to the Throwable)
    Athrow, //= 0xbf,
    /// load a byte or Boolean value from an array
    BALoad, //= 0x33
    /// store a byte or Boolean value into an array
    BAStore, //= 0x54
    /// push a byte onto the stack as an integer value
    BiPush(u8), //= 0x10
    /// reserved for breakpoints in Java debuggers; should not appear in any class file
    Breakpoint, //= 0xca
    /// load a char from an array
    CALoad, //= 0x34
    /// store a char into an array
    CAStore, //= 0x55
    /// checks whether an objectref is of a certain type, the class reference of which is in the constant pool at index (indexbyte1 << 8 + indexbyte2)
    Checkcast(u8, u8), //= 0xc0
    /// convert a double to a float
    D2f, //= 0x90
    /// convert a double to an int
    D2i, //= 0x8e
    /// convert a double to a long
    D2l, //= 0x8f
    /// add two doubles
    DAdd, //= 0x63
    /// load a double from an array
    DALoad, //= 0x31
    /// store a double into an array
    DAStore, //= 0x52
    /// compare two doubles
    Dcmpg, //= 0x98
    /// compare two doubles
    Dcmpl, //= 0x97
    /// push the constant 0.0 (a double) onto the stack
    DConst0, //= 0x0e
    /// push the constant 1.0 (a double) onto the stack
    DConst1, //= 0x0f
    /// divide two doubles
    Ddiv, //= 0x6f
    /// load a double value from a local variable #index
    Dload(u8), //= 0x18
    /// load a double from local variable 0
    Dload0, //= 0x26
    /// load a double from local variable 1
    Dload1, //= 0x27
    /// load a double from local variable 2
    Dload2, //= 0x28
    /// load a double from local variable 3
    Dload3, //= 0x29
    /// multiply two doubles
    Dmul, //= 0x6b
    /// negate a double
    Dneg, //= 0x77
    /// get the remainder from a division between two doubles
    Drem, //= 0x73
    /// return a double from a method
    Dreturn, //= 0xaf
    /// store a double value into a local variable #index
    Dstore(u8), //= 0x39
    /// store a double into local variable 0
    Dstore0, //= 0x47
    /// store a double into local variable 1
    Dstore1, //= 0x48
    /// store a double into local variable 2
    Dstore2, //= 0x49
    /// store a double into local variable 3
    Dstore3, //= 0x4a
    /// subtract a double from another
    Dsub, //= 0x67
    /// duplicate the value on top of the stack
    Dup, //= 0x59
    /// insert a copy of the top value into the stack two values from the top. value1 and value2 must not be of the type double or long.
    DupX1, //= 0x5a
    /// insert a copy of the top value into the stack two (if value2 is double or long it takes up the entry of value3, too) or three values (if value2 is neither double nor long) from the top
    DupX2, //= 0x5b
    /// duplicate top two stack words (two values, if value1 is not double nor long; a single value, if value1 is double or long)
    Dup2, //= 0x5c
    /// duplicate two words and insert beneath third word (see explanation above)
    Dup2X1, //= 0x5d
    /// duplicate two words and insert beneath fourth word
    Dup2X2, //= 0x5e
    /// convert a float to a double
    F2d, //= 0x8d
    /// convert a float to an int
    F2i, //= 0x8b
    /// convert a float to a long
    F2l, //= 0x8c
    /// add two floats
    Fadd, //= 0x62
    /// load a float from an array
    FALoad, //= 0x30
    /// store a float in an array
    FAStore, //= 0x51
    /// compare two floats
    Fcmpg, //= 0x96
    /// compare two floats
    Fcmpl, //= 0x95
    /// push 0.0f on the stack
    FConst0, //= 0x0b
    /// push 1.0f on the stack
    FConst1, //= 0x0c
    /// push 2.0f on the stack
    FConst2, //= 0x0d
    /// divide two floats
    Fdiv, //= 0x6e
    /// load a float value from a local variable #index
    Fload(u8), //= 0x17
    /// load a float value from local variable 0
    Fload0, //= 0x22
    /// load a float value from local variable 1
    Fload1, //= 0x23
    /// load a float value from local variable 2
    Fload2, //= 0x24
    /// load a float value from local variable 3
    Fload3, //= 0x25
    /// multiply two floats
    Fmul, //= 0x6a
    /// negate a float
    Fneg, //= 0x76
    /// get the remainder from a division between two floats
    Frem, //= 0x72
    /// return a float
    Freturn, //= 0xae
    /// store a float value into a local variable #index
    Fstore(u8), //= 0x38
    /// store a float value into local variable 0
    Fstore0, //= 0x43
    /// store a float value into local variable 1
    Fstore1, //= 0x44
    /// store a float value into local variable 2
    Fstore2, //= 0x45
    /// store a float value into local variable 3
    Fstore3, //= 0x46
    /// subtract two floats
    Fsub, //= 0x66
    /// get a field value of an object objectref, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)
    Getfield(u8, u8), //= 0xb4
    /// get a static field value of a class, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)
    Getstatic(u8, u8), //= 0xb2
    /// goes to another instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Goto(u8, u8), //= 0xa7
    /// goes to another instruction at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4)
    GotoW(u8, u8, u8, u8), //= 0xc8
    /// convert an int into a byte
    I2b, //= 0x91
    /// convert an int into a character
    I2c, //= 0x92
    /// convert an int into a double
    I2d, //= 0x87
    /// convert an int into a float
    I2f, //= 0x86
    /// convert an int into a long
    I2l, //= 0x85
    /// convert an int into a short
    I2s, //= 0x93
    /// add two ints
    Iadd, //= 0x60
    /// load an int from an array
    IALoad, //= 0x2e
    /// perform a bitwise AND on two integers
    Iand, //= 0x7e
    /// store an int into an array
    IAStore, //= 0x4f
    /// load the int value −1 onto the stack
    IconstM1, //= 0x02
    /// load the int value 0 onto the stack
    IConst0, //= 0x03
    /// load the int value 1 onto the stack
    IConst1, //= 0x04
    /// load the int value 2 onto the stack
    IConst2, //= 0x05
    /// load the int value 3 onto the stack
    IConst3, //= 0x06
    /// load the int value 4 onto the stack
    IConst4, //= 0x07
    /// load the int value 5 onto the stack
    IConst5, //= 0x08
    /// divide two integers
    Idiv, //= 0x6c
    /// if references are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfAcmpeq(u8, u8), //= 0xa5
    /// if references are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfAcmpne(u8, u8), //= 0xa6
    /// if ints are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpeq(u8, u8), //= 0x9f
    /// if value1 is greater than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpge(u8, u8), //= 0xa2
    /// if value1 is greater than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpgt(u8, u8), //= 0xa3
    /// if value1 is less than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmple(u8, u8), //= 0xa4
    /// if value1 is less than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmplt(u8, u8), //= 0xa1
    /// if ints are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpne(u8, u8), //= 0xa0
    /// if value is 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifeq(u8, u8), //= 0x99
    /// if value is greater than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifge(u8, u8), //= 0x9c
    /// if value is greater than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifgt(u8, u8), //= 0x9d
    /// if value is less than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifle(u8, u8), //= 0x9e
    /// if value is less than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Iflt(u8, u8), //= 0x9b
    /// if value is not 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifne(u8, u8), //= 0x9a
    /// if value is not null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifnonnull(u8, u8), //= 0xc7
    /// if value is null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifnull(u8, u8), //= 0xc6
    /// increment local variable #index by signed byte const
    Iinc(u8, u8), //= 0x84
    /// load an int value from a local variable #index
    Iload(u8), //= 0x15
    /// an int value from local variable 0
    Iload0, //= 0x1a
    /// an int value from local variable 1
    Iload1, //= 0x1b
    /// an int value from local variable 2
    Iload2, //= 0x1c
    /// an int value from local variable 3
    Iload3, //= 0x1d
    /// reserved for implementation-dependent operations within debuggers; should not appear in any class file
    Impdep1, //= 0xfe
    /// reserved for implementation-dependent operations within debuggers; should not appear in any class file
    Impdep2, //= 0xff
    /// multiply two integers
    Imul, //= 0x68
    /// negate int
    Ineg, //= 0x74
    /// determines if an object objectref is of a given type, identified by class reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InstanceOf(u8, u8), //= 0xc1
    /// invokes a dynamic method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeDynamic(u8, u8, u8, u8), //= 0xba
    /// invokes an interface method on object objectref and puts the result on the stack (might be void); the interface method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeInterface(u8, u8, u8, u8), //= 0xb9
    /// invoke instance method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeSpecial(u8, u8), //= 0xb7
    /// invoke a static method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeStatic(u8, u8), //= 0xb8
    /// invoke virtual method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeVirtual(u8, u8), //= 0xb6
    /// bitwise int OR
    Ior, //= 0x80
    /// logical int remainder
    Irem, //= 0x70
    /// return an integer from a method
    Ireturn, //= 0xac
    /// int shift left
    Ishl, //= 0x78
    /// int arithmetic shift right
    Ishr, //= 0x7a
    /// store int value into variable #index
    IStore(u8), //= 0x36
    /// store int value into variable 0
    IStore0, //= 0x3b
    /// store int value into variable 1
    IStore1, //= 0x3c
    /// store int value into variable 2
    IStore2, //= 0x3d
    /// store int value into variable 3
    IStore3, //= 0x3e
    /// int subtract
    Isub, //= 0x64
    /// int logical shift right
    Iushr, //= 0x7c
    /// int xor
    Ixor, //= 0x82
    /// jump to subroutine at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2) and place the return address on the stack
    Jsr(u8, u8), //= 0xa8
    /// jump to subroutine at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4) and place the return address on the stack
    JsrW(u8, u8, u8, u8), //= 0xc9
    /// convert a long to a double
    L2d, //= 0x8a
    /// convert a long to a float
    L2f, //= 0x89
    /// convert a long to a int
    L2i, //= 0x88
    /// add two longs
    Ladd, //= 0x61
    /// load a long from an array
    Laload, //= 0x2f
    /// bitwise AND of two longs
    Land, //= 0x7f
    /// store a long to an array
    Lastore, //= 0x50
    /// push 0 if the two longs are the same, 1 if value1 is greater than value2, -1 otherwise
    Lcmp, //= 0x94
    /// push 0L (the number zero with type long) onto the stack
    LConst0, //= 0x09
    /// push 1L (the number one with type long) onto the stack
    LConst1, //= 0x0a
    /// push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, java.lang.invoke.MethodHandle, or a dynamically-computed constant) onto the stack
    Ldc(u8), //= 0x12
    /// push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, java.lang.invoke.MethodHandle, or a dynamically-computed constant) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)
    LdcW(u8, u8), //= 0x13
    /// push a constant #index from a constant pool (double, long, or a dynamically-computed constant) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)
    Ldc2W(u8, u8), //= 0x14
    /// divide two longs
    Ldiv, //= 0x6d
    /// load a long value from a local variable #index
    Lload(u8), //= 0x16
    /// load a long value from a local variable 0
    LLoad0, //= 0x1e
    /// load a long value from a local variable 1
    LLoad1, //= 0x1f
    /// load a long value from a local variable 2
    LLoad2, //= 0x20
    /// load a long value from a local variable 3
    LLoad3, //= 0x21
    /// multiply two longs
    Lmul, //= 0x69
    /// negate a long
    Lneg, //= 0x75
    /// 8+: <0–3 bytes padding>, defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, npairs1, npairs2, npairs3, npairs4, match-offset pairs...	key →	a target address is looked up from a table using a key and execution continues from the instruction at that address
    Lookupswitch, //= 0xab
    /// bitwise OR of two longs
    Lor, //= 0x81
    /// remainder of division of two longs
    Lrem, //= 0x71
    /// return a long value
    Lreturn, //= 0xad
    /// bitwise shift left of a long value1 by int value2 positions
    Lshl, //= 0x79
    /// bitwise shift right of a long value1 by int value2 positions
    Lshr, //= 0x7b
    /// store a long value in a local variable #index
    Lstore(u8), //= 0x37
    /// store a long value in a local variable 0
    LStore0, //= 0x3f
    /// store a long value in a local variable 1
    LStore1, //= 0x40
    /// store a long value in a local variable 2
    LStore2, //= 0x41
    /// store a long value in a local variable 3
    LStore3, //= 0x42
    /// subtract two longs
    Lsub, //= 0x65
    /// bitwise shift right of a long value1 by int value2 positions, unsigned
    Lushr, //= 0x7d
    /// bitwise XOR of two longs
    Lxor, //= 0x83
    /// enter monitor for object ("grab the lock" – start of synchronized() section)
    MonitorEnter, //= 0xc2
    /// exit monitor for object ("release the lock" – end of synchronized() section)
    MonitorExit, //= 0xc3
    /// create a new array of dimensions dimensions of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2); the sizes of each dimension is identified by count1, [count2, etc.]
    MultiANewArray(u8, u8, u8), //= 0xc5
    /// create new object of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2)
    New(u8, u8), //= 0xbb
    /// create new array with count elements of primitive type identified by atype
    NewArray(u8), //= 0xbc
    /// perform no operation
    Nop, //= 0x00
    /// discard the top value on the stack
    Pop, //= 0x57
    /// discard the top two values on the stack (or one value, if it is a double or long)
    Pop2, //= 0x58
    /// set field to value in an object objectref, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    PutField(u8, u8), //= 0xb5
    /// set static field to value in a class, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    PutStatic(u8, u8), //= 0xb3
    /// continue execution from address taken from a local variable #index (the asymmetry with jsr is intentional)
    Ret(u8), //= 0xa9
    /// return void from method
    Return, //= 0xb1
    /// load short from array
    SAload, //= 0x35
    /// store short to array
    SAstore, //= 0x56
    /// push a short onto the stack as an integer value
    SIPush(u8, u8), //= 0x11
    /// swaps two top words on the stack (note that value1 and value2 must not be double or long)
    Swap, //= 0x5f
    /// 16+: [0–3 bytes padding], defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, lowbyte1, lowbyte2, lowbyte3, lowbyte4, highbyte1, highbyte2, highbyte3, highbyte4, jump offsets...	index →	continue execution from an address in the table at offset index
    TableSwitch, //= 0xaa
    /// 3/5: opcode, indexbyte1, indexbyte2, indexbyte2, countbyte1, countbyte2	[same as for corresponding instructions] execute opcode, where opcode is either iload, fload, aload, lload, dload, istore, fstore, astore, lstore, dstore, or ret, but assume the index is 16 bit; or execute iinc, where the index is 16 bits and the constant to increment by is a signed 16 bit short
    Wide, //= 0xc4or
    NoName, //= 0xcb..=0xfd
}