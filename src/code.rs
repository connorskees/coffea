use std::slice::Iter;

use crate::attributes::{Attribute, ExceptionTableEntry};

#[derive(Debug, Clone)]
pub struct Instructions<'a> {
    bytes: Iter<'a, u8>,
}

impl Instructions<'_> {
    fn next_byte(&mut self) -> Option<u8> {
        self.bytes.next().cloned()
    }
}

impl Iterator for Instructions<'_> {
    type Item = Instruction;
    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.bytes.next()? {
            0x32 => Instruction::AALoad,
            0x53 => Instruction::AAStore,
            0x01 => Instruction::AConstNull,
            0x19 => Instruction::ALoad(self.next_byte()?),
            0x2a => Instruction::Aload0,
            0x2b => Instruction::Aload1,
            0x2c => Instruction::Aload2,
            0x2d => Instruction::ALoad3,
            0xbd => {
                Instruction::ANewArray(u16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xb0 => Instruction::AReturn,
            0xbe => Instruction::ArrayLength,
            0x3a => Instruction::AStore(self.next_byte()?),
            0x4b => Instruction::AStore0,
            0x4c => Instruction::AStore1,
            0x4d => Instruction::AStore2,
            0x4e => Instruction::AStore3,
            0xbf => Instruction::Athrow,
            0x33 => Instruction::BALoad,
            0x54 => Instruction::BAStore,
            0x10 => Instruction::BiPush(self.next_byte()?),
            0xca => Instruction::Breakpoint,
            0x34 => Instruction::CALoad,
            0x55 => Instruction::CAStore,
            0xc0 => {
                Instruction::Checkcast(u16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
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
            0x6f => Instruction::DDiv,
            0x18 => Instruction::DLoad(self.next_byte()?),
            0x26 => Instruction::DLoad0,
            0x27 => Instruction::DLoad1,
            0x28 => Instruction::DLoad2,
            0x29 => Instruction::DLoad3,
            0x6b => Instruction::DMul,
            0x77 => Instruction::DNeg,
            0x73 => Instruction::DRem,
            0xaf => Instruction::Dreturn,
            0x39 => Instruction::DStore(self.next_byte()?),
            0x47 => Instruction::DStore0,
            0x48 => Instruction::DStore1,
            0x49 => Instruction::DStore2,
            0x4a => Instruction::DStore3,
            0x67 => Instruction::DSub,
            0x59 => Instruction::Dup,
            0x5a => Instruction::DupX1,
            0x5b => Instruction::DupX2,
            0x5c => Instruction::Dup2,
            0x5d => Instruction::Dup2X1,
            0x5e => Instruction::Dup2X2,
            0x8d => Instruction::F2d,
            0x8b => Instruction::F2i,
            0x8c => Instruction::F2l,
            0x62 => Instruction::FAdd,
            0x30 => Instruction::FALoad,
            0x51 => Instruction::FAStore,
            0x96 => Instruction::Fcmpg,
            0x95 => Instruction::Fcmpl,
            0x0b => Instruction::FConst0,
            0x0c => Instruction::FConst1,
            0x0d => Instruction::FConst2,
            0x6e => Instruction::FDiv,
            0x17 => Instruction::FLoad(self.next_byte()?),
            0x22 => Instruction::FLoad0,
            0x23 => Instruction::FLoad1,
            0x24 => Instruction::FLoad2,
            0x25 => Instruction::FLoad3,
            0x6a => Instruction::FMul,
            0x76 => Instruction::FNeg,
            0x72 => Instruction::FRem,
            0xae => Instruction::Freturn,
            0x38 => Instruction::FStore(self.next_byte()?),
            0x43 => Instruction::FStore0,
            0x44 => Instruction::FStore1,
            0x45 => Instruction::FStore2,
            0x46 => Instruction::FStore3,
            0x66 => Instruction::FSub,
            0xb4 => {
                Instruction::GetField(u16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xb2 => {
                Instruction::GetStatic(u16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa7 => Instruction::Goto(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0xc8 => Instruction::GotoW(i32::from_be_bytes([
                self.next_byte()?,
                self.next_byte()?,
                self.next_byte()?,
                self.next_byte()?,
            ])),
            0x91 => Instruction::I2b,
            0x92 => Instruction::I2c,
            0x87 => Instruction::I2d,
            0x86 => Instruction::I2f,
            0x85 => Instruction::I2l,
            0x93 => Instruction::I2s,
            0x60 => Instruction::IAdd,
            0x2e => Instruction::IALoad,
            0x7e => Instruction::IAnd,
            0x4f => Instruction::IAStore,
            0x02 => Instruction::IConstM1,
            0x03 => Instruction::IConst0,
            0x04 => Instruction::IConst1,
            0x05 => Instruction::IConst2,
            0x06 => Instruction::IConst3,
            0x07 => Instruction::IConst4,
            0x08 => Instruction::IConst5,
            0x6c => Instruction::IDiv,
            0xa5 => {
                Instruction::IfAcmpeq(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa6 => {
                Instruction::IfAcmpne(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0x9f => {
                Instruction::IfIcmpeq(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa2 => {
                Instruction::IfIcmpge(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa3 => {
                Instruction::IfIcmpgt(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa4 => {
                Instruction::IfIcmple(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa1 => {
                Instruction::IfIcmplt(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa0 => {
                Instruction::IfIcmpne(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0x99 => Instruction::Ifeq(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x9c => Instruction::Ifge(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x9d => Instruction::Ifgt(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x9e => Instruction::Ifle(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x9b => Instruction::Iflt(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x9a => Instruction::Ifne(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0xc7 => {
                Instruction::Ifnonnull(i16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xc6 => Instruction::Ifnull(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x84 => Instruction::Iinc(self.next_byte()?, self.next_byte()?),
            0x15 => Instruction::ILoad(self.next_byte()?),
            0x1a => Instruction::ILoad0,
            0x1b => Instruction::ILoad1,
            0x1c => Instruction::ILoad2,
            0x1d => Instruction::ILoad3,
            0xfe => Instruction::Impdep1,
            0xff => Instruction::Impdep2,
            0x68 => Instruction::IMul,
            0x74 => Instruction::INeg,
            0xc1 => {
                Instruction::InstanceOf(u16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xba => Instruction::InvokeDynamic(
                u16::from_be_bytes([self.next_byte()?, self.next_byte()?]),
                self.next_byte()?,
                self.next_byte()?,
            ),
            0xb9 => Instruction::InvokeInterface(
                u16::from_be_bytes([self.next_byte()?, self.next_byte()?]),
                self.next_byte()?,
                self.next_byte()?,
            ),
            0xb7 => Instruction::InvokeSpecial(u16::from_be_bytes([
                self.next_byte()?,
                self.next_byte()?,
            ])),
            0xb8 => Instruction::InvokeStatic(u16::from_be_bytes([
                self.next_byte()?,
                self.next_byte()?,
            ])),
            0xb6 => Instruction::InvokeVirtual(u16::from_be_bytes([
                self.next_byte()?,
                self.next_byte()?,
            ])),
            0x80 => Instruction::IOr,
            0x70 => Instruction::IRem,
            0xac => Instruction::Ireturn,
            0x78 => Instruction::IShl,
            0x7a => Instruction::IShr,
            0x36 => Instruction::IStore(self.next_byte()?),
            0x3b => Instruction::IStore0,
            0x3c => Instruction::IStore1,
            0x3d => Instruction::IStore2,
            0x3e => Instruction::IStore3,
            0x64 => Instruction::ISub,
            0x7c => Instruction::IUShr,
            0x82 => Instruction::IXor,
            0xa8 => Instruction::Jsr(i16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0xc9 => Instruction::JsrW(i32::from_be_bytes([
                self.next_byte()?,
                self.next_byte()?,
                self.next_byte()?,
                self.next_byte()?,
            ])),
            0x8a => Instruction::L2d,
            0x89 => Instruction::L2f,
            0x88 => Instruction::L2i,
            0x61 => Instruction::LAdd,
            0x2f => Instruction::LALoad,
            0x7f => Instruction::LAnd,
            0x50 => Instruction::LAStore,
            0x94 => Instruction::Lcmp,
            0x09 => Instruction::LConst0,
            0x0a => Instruction::LConst1,
            0x12 => Instruction::Ldc(u16::from(self.next_byte()?)),
            0x13 => Instruction::LdcW(u16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x14 => Instruction::Ldc2W(u16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x6d => Instruction::LDiv,
            0x16 => Instruction::LLoad(self.next_byte()?),
            0x1e => Instruction::LLoad0,
            0x1f => Instruction::LLoad1,
            0x20 => Instruction::LLoad2,
            0x21 => Instruction::LLoad3,
            0x69 => Instruction::LMul,
            0x75 => Instruction::LNeg,
            0xab => unimplemented!("instruction `LookupSwitch` not yet implemented"), //Instruction::Lookupswitch,
            0x81 => Instruction::LOr,
            0x71 => Instruction::LRem,
            0xad => Instruction::Lreturn,
            0x79 => Instruction::LShl,
            0x7b => Instruction::LShr,
            0x37 => Instruction::LStore(self.next_byte()?),
            0x3f => Instruction::LStore0,
            0x40 => Instruction::LStore1,
            0x41 => Instruction::LStore2,
            0x42 => Instruction::LStore3,
            0x65 => Instruction::LSub,
            0x7d => Instruction::LUShr,
            0x83 => Instruction::LXor,
            0xc2 => Instruction::MonitorEnter,
            0xc3 => Instruction::MonitorExit,
            0xc5 => {
                Instruction::MultiANewArray(self.next_byte()?, self.next_byte()?, self.next_byte()?)
            }
            0xbb => Instruction::New(u16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0xbc => Instruction::NewArray(self.next_byte()?),
            0x00 => Instruction::Nop,
            0x57 => Instruction::Pop,
            0x58 => Instruction::Pop2,
            0xb5 => {
                Instruction::PutField(u16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xb3 => {
                Instruction::PutStatic(u16::from_be_bytes([self.next_byte()?, self.next_byte()?]))
            }
            0xa9 => Instruction::Ret(self.next_byte()?),
            0xb1 => Instruction::Return,
            0x35 => Instruction::SALoad,
            0x56 => Instruction::SAStore,
            0x11 => Instruction::SIPush(u16::from_be_bytes([self.next_byte()?, self.next_byte()?])),
            0x5f => Instruction::Swap,
            0xaa => unimplemented!("instruction `TableSwitch` not yet implemented"), //Instruction::TableSwitch,
            0xc4 => {
                let n = self.next_byte()?;
                match n {
                    0x84 => Instruction::Wide5(n, self.next_byte()?, self.next_byte()?, self.next_byte()?, self.next_byte()?),
                    0x15..=0x19 | 0x36..=0x39 | 0x89 => Instruction::Wide3(n, self.next_byte()?, self.next_byte()?),
                    _ => unimplemented!("invalid opcode in `Instruction::Wide3`; error handling not yet implemented"),
                }
            }
            0xcb..=0xfd => Instruction::NoName,
        })
    }
}

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
        Instructions {
            bytes: self.code.iter(),
        }
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

// todo: standardize instruction casing
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
    ANewArray(u16),
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
    Checkcast(u16),
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
    DDiv,
    /// load a double value from a local variable #index
    DLoad(u8),
    /// load a double from local variable 0
    DLoad0,
    /// load a double from local variable 1
    DLoad1,
    /// load a double from local variable 2
    DLoad2,
    /// load a double from local variable 3
    DLoad3,
    /// multiply two doubles
    DMul,
    /// negate a double
    DNeg,
    /// get the remainder from a division between two doubles
    DRem,
    /// return a double from a method
    Dreturn,
    /// store a double value into a local variable #index
    DStore(u8),
    /// store a double into local variable 0
    DStore0,
    /// store a double into local variable 1
    DStore1,
    /// store a double into local variable 2
    DStore2,
    /// store a double into local variable 3
    DStore3,
    /// subtract a double from another
    DSub,
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
    FAdd,
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
    FDiv,
    /// load a float value from a local variable #index
    FLoad(u8),
    /// load a float value from local variable 0
    FLoad0,
    /// load a float value from local variable 1
    FLoad1,
    /// load a float value from local variable 2
    FLoad2,
    /// load a float value from local variable 3
    FLoad3,
    /// multiply two floats
    FMul,
    /// negate a float
    FNeg,
    /// get the remainder from a division between two floats
    FRem,
    /// return a float
    Freturn,
    /// store a float value into a local variable #index
    FStore(u8),
    /// store a float value into local variable 0
    FStore0,
    /// store a float value into local variable 1
    FStore1,
    /// store a float value into local variable 2
    FStore2,
    /// store a float value into local variable 3
    FStore3,
    /// subtract two floats
    FSub,
    /// get a field value of an object objectref, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)
    GetField(u16),
    /// get a static field value of a class, where the field is identified by field reference in the constant pool index (indexbyte1 << 8 + indexbyte2)
    GetStatic(u16),
    /// goes to another instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Goto(i16),
    /// goes to another instruction at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4)
    GotoW(i32),
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
    IAdd,
    /// load an int from an array
    IALoad,
    /// perform a bitwise AND on two integers
    IAnd,
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
    IDiv,
    /// if references are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfAcmpeq(i16),
    /// if references are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfAcmpne(i16),
    /// if ints are equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpeq(i16),
    /// if value1 is greater than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpge(i16),
    /// if value1 is greater than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpgt(i16),
    /// if value1 is less than or equal to value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmple(i16),
    /// if value1 is less than value2, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmplt(i16),
    /// if ints are not equal, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    IfIcmpne(i16),
    /// if value is 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifeq(i16),
    /// if value is greater than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifge(i16),
    /// if value is greater than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifgt(i16),
    /// if value is less than or equal to 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifle(i16),
    /// if value is less than 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Iflt(i16),
    /// if value is not 0, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifne(i16),
    /// if value is not null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifnonnull(i16),
    /// if value is null, branch to instruction at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2)
    Ifnull(i16),
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
    IMul,
    /// negate int
    INeg,
    /// determines if an object objectref is of a given type, identified by class reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InstanceOf(u16),
    /// invokes a dynamic method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeDynamic(u16, u8, u8),
    /// invokes an interface method on object objectref and puts the result on the stack (might be void); the interface method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeInterface(u16, u8, u8),
    /// invoke instance method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeSpecial(u16),
    /// invoke a static method and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeStatic(u16),
    /// invoke virtual method on object objectref and puts the result on the stack (might be void); the method is identified by method reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    InvokeVirtual(u16),
    /// bitwise int OR
    IOr,
    /// logical int remainder
    IRem,
    /// return an integer from a method
    Ireturn,
    /// int shift left
    IShl,
    /// int arithmetic shift right
    IShr,
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
    ISub,
    /// int logical shift right
    IUShr,
    /// int xor
    IXor,
    /// jump to subroutine at branchoffset (signed short constructed from unsigned bytes branchbyte1 << 8 + branchbyte2) and place the return address on the stack
    Jsr(i16),
    /// jump to subroutine at branchoffset (signed int constructed from unsigned bytes branchbyte1 << 24 + branchbyte2 << 16 + branchbyte3 << 8 + branchbyte4) and place the return address on the stack
    JsrW(i32),
    /// convert a long to a double
    L2d,
    /// convert a long to a float
    L2f,
    /// convert a long to a int
    L2i,
    /// add two longs
    LAdd,
    /// load a long from an array
    LALoad,
    /// bitwise AND of two longs
    LAnd,
    /// store a long to an array
    LAStore,
    /// push 0 if the two longs are the same, 1 if value1 is greater than value2, -1 otherwise
    Lcmp,
    /// push 0L (the number zero with type long) onto the stack
    LConst0,
    /// push 1L (the number one with type long) onto the stack
    LConst1,
    /// push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, java.lang.invoke.MethodHandle, or a dynamically-computed constant) onto the stack
    Ldc(u16),
    /// push a constant #index from a constant pool (String, int, float, Class, java.lang.invoke.MethodType, java.lang.invoke.MethodHandle, or a dynamically-computed constant) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)
    LdcW(u16),
    /// push a constant #index from a constant pool (double, long, or a dynamically-computed constant) onto the stack (wide index is constructed as indexbyte1 << 8 + indexbyte2)
    Ldc2W(u16),
    /// divide two longs
    LDiv,
    /// load a long value from a local variable #index
    LLoad(u8),
    /// load a long value from a local variable 0
    LLoad0,
    /// load a long value from a local variable 1
    LLoad1,
    /// load a long value from a local variable 2
    LLoad2,
    /// load a long value from a local variable 3
    LLoad3,
    /// multiply two longs
    LMul,
    /// negate a long
    LNeg,
    /// 8+: <0–3 bytes padding>, defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, npairs1, npairs2, npairs3, npairs4, match-offset pairs...	key →	a target address is looked up from a table using a key and execution continues from the instruction at that address
    Lookupswitch,
    /// bitwise OR of two longs
    LOr,
    /// remainder of division of two longs
    LRem,
    /// return a long value
    Lreturn,
    /// bitwise shift left of a long value1 by int value2 positions
    LShl,
    /// bitwise shift right of a long value1 by int value2 positions
    LShr,
    /// store a long value in a local variable #index
    LStore(u8),
    /// store a long value in a local variable 0
    LStore0,
    /// store a long value in a local variable 1
    LStore1,
    /// store a long value in a local variable 2
    LStore2,
    /// store a long value in a local variable 3
    LStore3,
    /// subtract two longs
    LSub,
    /// bitwise shift right of a long value1 by int value2 positions, unsigned
    LUShr,
    /// bitwise XOR of two longs
    LXor,
    /// enter monitor for object ("grab the lock" – start of synchronized() section)
    MonitorEnter,
    /// exit monitor for object ("release the lock" – end of synchronized() section)
    MonitorExit,
    /// create a new array of dimensions dimensions of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2); the sizes of each dimension is identified by count1, [count2, etc.]
    MultiANewArray(u8, u8, u8),
    /// create new object of type identified by class reference in constant pool index (indexbyte1 << 8 + indexbyte2)
    New(u16),
    /// create new array with count elements of primitive type identified by atype
    NewArray(u8),
    /// perform no operation
    Nop,
    /// discard the top value on the stack
    Pop,
    /// discard the top two values on the stack (or one value, if it is a double or long)
    Pop2,
    /// set field to value in an object objectref, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    PutField(u16),
    /// set static field to value in a class, where the field is identified by a field reference index in constant pool (indexbyte1 << 8 + indexbyte2)
    PutStatic(u16),
    /// continue execution from address taken from a local variable #index (the asymmetry with jsr is intentional)
    Ret(u8),
    /// return void from method
    Return,
    /// load short from array
    SALoad,
    /// store short to array
    SAStore,
    /// push a short onto the stack as an integer value
    SIPush(u16),
    /// swaps two top words on the stack (note that value1 and value2 must not be double or long)
    Swap,
    /// 16+: [0–3 bytes padding], defaultbyte1, defaultbyte2, defaultbyte3, defaultbyte4, lowbyte1, lowbyte2, lowbyte3, lowbyte4, highbyte1, highbyte2, highbyte3, highbyte4, jump offsets...	index →	continue execution from an address in the table at offset index
    TableSwitch,
    ///  execute opcode, where opcode is either iload, fLoad, aload, lload, dLoad, istore, fstore, astore, lstore, dstore, or ret, but assume the index is 16 bit
    Wide3(u8, u8, u8),
    /// execute iinc, where the index is 16 bits and the constant to increment by is a signed 16 bit short
    Wide5(u8, u8, u8, u8, u8),
    NoName, //= 0xcb..=0xfd
}

#[repr(u8)]
pub(crate) enum InstLen {
    One = 1,
    Two = 2,
    Three = 3,
    Four = 4,
    Five = 5,
    Six = 6,
}

impl Instruction {
    // todo: potentially
    //  - `TableSwitch`
    //  - `Ret`
    //  - `Lookupswitch`
    //  - `Jsr`
    //  - `JsrW`
    pub(crate) fn is_control_flow(&self) -> bool {
        matches!(
            self,
            Instruction::Goto(..)
            | Instruction::GotoW(_)
            | Instruction::IfAcmpeq(_)
            | Instruction::IfAcmpne(_)
            | Instruction::IfIcmpeq(_)
            | Instruction::IfIcmpge(_)
            | Instruction::IfIcmpgt(_)
            | Instruction::IfIcmple(_)
            | Instruction::IfIcmplt(_)
            | Instruction::IfIcmpne(_)
            | Instruction::Ifeq(_)
            | Instruction::Ifge(_)
            | Instruction::Ifgt(_)
            | Instruction::Ifle(_)
            | Instruction::Iflt(_)
            | Instruction::Ifne(_)
            | Instruction::Ifnonnull(_)
            | Instruction::Ifnull(_)
            | Instruction::Return
            | Instruction::AReturn
            | Instruction::Dreturn
            | Instruction::Freturn
            | Instruction::Ireturn
            | Instruction::Lreturn
        )
    }

    #[must_use]
    // todo: some have u16 so should count for 3
    pub(crate) fn len(self) -> InstLen {
        match self {
            Instruction::AALoad
            | Instruction::AAStore
            | Instruction::AConstNull
            | Instruction::Aload0
            | Instruction::Aload1
            | Instruction::Aload2
            | Instruction::ALoad3
            | Instruction::AReturn
            | Instruction::ArrayLength
            | Instruction::AStore0
            | Instruction::AStore1
            | Instruction::AStore2
            | Instruction::AStore3
            | Instruction::Athrow
            | Instruction::BALoad
            | Instruction::BAStore
            | Instruction::Breakpoint
            | Instruction::CALoad
            | Instruction::CAStore
            | Instruction::D2f
            | Instruction::D2i
            | Instruction::D2l
            | Instruction::DAdd
            | Instruction::DALoad
            | Instruction::DAStore
            | Instruction::Dcmpg
            | Instruction::Dcmpl
            | Instruction::DConst0
            | Instruction::DConst1
            | Instruction::DDiv
            | Instruction::DLoad0
            | Instruction::DLoad1
            | Instruction::DLoad2
            | Instruction::DLoad3
            | Instruction::DMul
            | Instruction::DNeg
            | Instruction::DRem
            | Instruction::Dreturn
            | Instruction::DStore0
            | Instruction::DStore1
            | Instruction::DStore2
            | Instruction::DStore3
            | Instruction::DSub
            | Instruction::Dup
            | Instruction::DupX1
            | Instruction::DupX2
            | Instruction::Dup2
            | Instruction::Dup2X1
            | Instruction::Dup2X2
            | Instruction::F2d
            | Instruction::F2i
            | Instruction::F2l
            | Instruction::FAdd
            | Instruction::FALoad
            | Instruction::FAStore
            | Instruction::Fcmpg
            | Instruction::Fcmpl
            | Instruction::FConst0
            | Instruction::FConst1
            | Instruction::FConst2
            | Instruction::FDiv
            | Instruction::FLoad0
            | Instruction::FLoad1
            | Instruction::FLoad2
            | Instruction::FLoad3
            | Instruction::FMul
            | Instruction::FNeg
            | Instruction::FRem
            | Instruction::Freturn
            | Instruction::FStore0
            | Instruction::FStore1
            | Instruction::FStore2
            | Instruction::FStore3
            | Instruction::FSub
            | Instruction::I2b
            | Instruction::I2c
            | Instruction::I2d
            | Instruction::I2f
            | Instruction::I2l
            | Instruction::I2s
            | Instruction::IAdd
            | Instruction::IALoad
            | Instruction::IAnd
            | Instruction::IAStore
            | Instruction::IConstM1
            | Instruction::IConst0
            | Instruction::IConst1
            | Instruction::IConst2
            | Instruction::IConst3
            | Instruction::IConst4
            | Instruction::IConst5
            | Instruction::IDiv
            | Instruction::ILoad0
            | Instruction::ILoad1
            | Instruction::ILoad2
            | Instruction::ILoad3
            | Instruction::Impdep1
            | Instruction::Impdep2
            | Instruction::IMul
            | Instruction::INeg
            | Instruction::IOr
            | Instruction::IRem
            | Instruction::Ireturn
            | Instruction::IShl
            | Instruction::IShr
            | Instruction::IStore0
            | Instruction::IStore1
            | Instruction::IStore2
            | Instruction::IStore3
            | Instruction::ISub
            | Instruction::IUShr
            | Instruction::IXor
            | Instruction::L2d
            | Instruction::L2f
            | Instruction::L2i
            | Instruction::LAdd
            | Instruction::LALoad
            | Instruction::LAnd
            | Instruction::LAStore
            | Instruction::Lcmp
            | Instruction::LConst0
            | Instruction::LConst1
            | Instruction::LDiv
            | Instruction::LLoad0
            | Instruction::LLoad1
            | Instruction::LLoad2
            | Instruction::LLoad3
            | Instruction::LMul
            | Instruction::LNeg
            | Instruction::LOr
            | Instruction::LRem
            | Instruction::Lreturn
            | Instruction::LShl
            | Instruction::LShr
            | Instruction::LStore0
            | Instruction::LStore1
            | Instruction::LStore2
            | Instruction::LStore3
            | Instruction::LSub
            | Instruction::LUShr
            | Instruction::LXor
            | Instruction::MonitorEnter
            | Instruction::MonitorExit
            | Instruction::Nop
            | Instruction::Pop
            | Instruction::Pop2
            | Instruction::Return
            | Instruction::SALoad
            | Instruction::SAStore
            | Instruction::Swap
            | Instruction::NoName => InstLen::One,
            Instruction::ALoad(_)
            | Instruction::AStore(_)
            | Instruction::BiPush(_)
            | Instruction::DLoad(_)
            | Instruction::DStore(_)
            | Instruction::FLoad(_)
            | Instruction::FStore(_)
            | Instruction::GetField(_)
            | Instruction::GetStatic(_)
            | Instruction::ILoad(_)
            | Instruction::InvokeStatic(_)
            | Instruction::InvokeVirtual(_)
            | Instruction::IStore(_)
            | Instruction::LStore(_)
            | Instruction::NewArray(_)
            | Instruction::Ret(_)
            | Instruction::SIPush(_)
            | Instruction::Ldc(_)
            | Instruction::LLoad(_) => InstLen::Two,
            Instruction::ANewArray(_)
            | Instruction::Checkcast(_)
            | Instruction::Goto(_)
            | Instruction::InvokeSpecial(_)
            | Instruction::IfAcmpeq(_)
            | Instruction::IfAcmpne(_)
            | Instruction::IfIcmpeq(_)
            | Instruction::IfIcmpge(_)
            | Instruction::IfIcmpgt(_)
            | Instruction::IfIcmple(_)
            | Instruction::IfIcmplt(_)
            | Instruction::IfIcmpne(_)
            | Instruction::Ifeq(_)
            | Instruction::Ifge(_)
            | Instruction::Ifgt(_)
            | Instruction::Ifle(_)
            | Instruction::Iflt(_)
            | Instruction::Ifne(_)
            | Instruction::Ifnonnull(_)
            | Instruction::Ifnull(_)
            | Instruction::Iinc(_, _)
            | Instruction::InstanceOf(_)
            | Instruction::Jsr(_)
            | Instruction::LdcW(_)
            | Instruction::Ldc2W(_)
            | Instruction::New(_)
            | Instruction::PutField(_)
            | Instruction::PutStatic(_) => InstLen::Three,
            Instruction::Wide3(_, _, _) | Instruction::MultiANewArray(_, _, _) => InstLen::Four,
            Instruction::GotoW(_)
            | Instruction::InvokeDynamic(_, _, _)
            | Instruction::InvokeInterface(_, _, _)
            | Instruction::JsrW(_) => InstLen::Five,
            Instruction::Wide5(_, _, _, _, _) => InstLen::Six,
            Instruction::TableSwitch => {
                unimplemented!("instruction `TableSwitch` not yet implemented")
            }
            Instruction::Lookupswitch => {
                unimplemented!("instruction `LookupSwitch` not yet implemented")
            } //Instruction::Lookupswitch,
        }
    }
}
