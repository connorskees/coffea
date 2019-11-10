use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead, BufReader, Read};

const TEST_CLASS_FILE_PATH: &str = "test.class";

enum PoolKind {
    Class = 7,
    Fieldref = 9,
    Methodref = 10,
    InterfaceMethodref = 11,
    String = 8,
    Integer = 3,
    Float = 4,
    Long = 5,
    Double = 6,
    NameAndType = 12,
    Utf8 = 1,
    MethodHandle = 15,
    MethodType = 16,
    InvokeDynamic = 18,
}

enum AccessFlags {
    Public = 0x0001,
    Final = 0x0010,
    Super = 0x0020,
    Interface = 0x0200,
    Abstract = 0x0400,
    Synthetic = 0x1000,
    Annotation = 0x2000,
    Enum = 0x4000,
}

struct AttributeInfo {
    attribute_name_index: [u8; 2],
    attribute_length: [u8; 4],
    info: Vec<u8>,
}

struct FieldInfo {
    access_flags: AccessFlags,
    name_index: [u8; 2],
    descriptor_index: [u8; 2],
    attributes_count: [u8; 2],
    attribute_info: Vec<AttributeInfo>,
}

struct MethodInfo {
    access_flags: [u8; 2],
    name_index: [u8; 2],
    descriptor_index: [u8; 2],
    attributes_count: [u8; 2],
    attributes: Vec<AttributeInfo>,
}

struct ConstantPoolInfo {
    pool_kind: PoolKind,
    bytes: Vec<u8>,
}

struct ClassFile {
    magic: [u8; 4],
    minor_version: [u8; 2],
    major_version: [u8; 2],
    constant_pool_count: [u8; 2],
    constant_pool: Vec<ConstantPoolInfo>,
    access_flags: [u8; 2],
    this_class: [u8; 2],
    super_class: [u8; 2],
    interfaces_count: [u8; 2],
    interfaces: Vec<u8>,
    fields_count: [u8; 2],
    fields: Vec<FieldInfo>,
    methods_count: [u8; 2],
    methods: Vec<MethodInfo>,
    attributes_count: [u8; 2],
    attributes: Vec<AttributeInfo>,
}

/// Read `n` bytes as [u8; n]
macro_rules! read_bytes_to_buffer {
    ($reader:ident, $bytes:literal) => {
        if let Some(mut buffer) = Some([0u8; $bytes]) {
            $reader.read_exact(&mut buffer)?;
            // u32::from_be_bytes(buffer).to_be_bytes()
            buffer
        } else {
            unreachable!()
        }
    };
}

/// Read a single byte as a u8
macro_rules! read_u8 {
    ($reader:ident) => {
        if let Some(mut buffer) = Some([0u8]) {
            $reader.read_exact(&mut buffer)?;
            u8::from_be_bytes(buffer)
        } else {
            unreachable!()
        }
    };
}

/// Read 2 bytes as a u16
macro_rules! read_u16 {
    ($reader:ident) => {
        if let Some(mut buffer) = Some([0u8; 2]) {
            $reader.read_exact(&mut buffer)?;
            u16::from_be_bytes(buffer)
        } else {
            unreachable!()
        }
    };
}

/// Read 4 bytes as a u32
macro_rules! read_u32 {
    ($reader:ident) => {
        if let Some(mut buffer) = Some([0u8; 4]) {
            $reader.read_exact(&mut buffer)?;
            u32::from_be_bytes(buffer)
        } else {
            unreachable!()
        }
    };
}

fn main() -> io::Result<()> {
    let mut reader = BufReader::new(File::open(TEST_CLASS_FILE_PATH)?);
    let magic_number = read_u32!(reader);
    println!("{}", magic_number.to_string());
    assert_eq!(magic_number, 0xCAFEBABE);

    dbg!(magic_number);

    Ok(())
}
