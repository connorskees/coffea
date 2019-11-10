use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead, BufReader, Read};

const TEST_CLASS_FILE_PATH: &str = "test.class";
const CLASS_FILE_HEADER: [u8; 4] = [0xCA, 0xFE, 0xBA, 0xBE];

enum PoolKind {
    Class {
        name_index: u16,
    },
    FieldRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    MethodRef {
        class_index: u16,
        name_and_type_index: u16,
    },
    InterfaceMethodref {
        class_index: u16,
        name_and_type_index: u16,
    },
    String {
        string_index: u16,
    },
    Integer {
        bytes: u32,
    },
    Float {
        bytes: u32,
    },
    Long {
        high_bytes: u32,
        low_bytes: u32,
    },
    Double {
        high_bytes: u32,
        low_bytes: u32,
    },
    NameAndType {
        name_index: u16,
        descriptor_index: u16,
    },
    Utf8 {
        bytes: Vec<u8>,
    },
    MethodHandle {
        reference_kind: u8,
        reference_index: u16,
    },
    MethodType {
        descriptor_index: u16,
    },
    InvokeDynamic {
        boostrap_method_attr_index: u16,
        name_and_type_index: u16,
    },
}

enum FieldAccessFlags {
    Public = 0x0001,
    Private = 0x0002,
    Protected = 0x0004,
    Static = 0x0008,
    Final = 0x0010,
    Volatile = 0x0040,
    Transient = 0x0080,
    Synthetic = 0x1000,
    Enum = 0x4000,
}

enum MethodAccessFlags {
    Public = 0x0001,
    Private = 0x0002,
    Protected = 0x0004,
    Static = 0x0008,
    Final = 0x0010,
    Synchronized = 0x0020,
    Bridge = 0x0040,
    VarArgs = 0x0080,
    Native = 0x0100,
    Abstract = 0x0400,
    Strict = 0x0800,
    Synthetic = 0x1000,
}

struct AttributeInfo {
    attribute_name_index: u16,
    attribute_length: u32,
    info: Vec<u8>,
}

struct FieldInfo {
    access_flags: FieldAccessFlags,
    name_index: u16,
    descriptor_index: u16,
    attribute_info: Vec<AttributeInfo>,
}

struct MethodInfo {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<AttributeInfo>,
}

struct ConstantPoolInfo {
    pool_kind: PoolKind,
    bytes: Vec<u8>,
}

pub enum MajorVersion {
    JavaSE14 = 58,
    JavaSE13 = 57,
    JavaSE12 = 56,
    JavaSE11 = 55,
    JavaSE10 = 54,
    JavaSE9 = 53,
    JavaSE8 = 52,
    JavaSE7 = 51,
    JavaSE6 = 50,
    JavaSE5 = 49,
    JDK1_4 = 48,
    JDK1_3 = 47,
    JDK1_2 = 46,
    JDK1_1 = 45,
}

impl MajorVersion {
    pub fn from_u16(n: u16) -> MajorVersion {
        match n {
            58 => MajorVersion::JavaSE14,
            57 => MajorVersion::JavaSE13,
            56 => MajorVersion::JavaSE12,
            55 => MajorVersion::JavaSE11,
            54 => MajorVersion::JavaSE10,
            53 => MajorVersion::JavaSE9,
            52 => MajorVersion::JavaSE8,
            51 => MajorVersion::JavaSE7,
            50 => MajorVersion::JavaSE6,
            49 => MajorVersion::JavaSE5,
            48 => MajorVersion::JDK1_4,
            47 => MajorVersion::JDK1_3,
            46 => MajorVersion::JDK1_2,
            45 => MajorVersion::JDK1_1,
            _ => unimplemented!(),
        }
    }
}

struct ClassFile {
    version: (MajorVersion, u16),
    constant_pool: Vec<ConstantPoolInfo>,
    access_flags: u16,
    this_class: u16,
    super_class: u16,
    interfaces: Vec<u8>,
    fields: Vec<FieldInfo>,
    methods: Vec<MethodInfo>,
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
    assert_eq!(read_bytes_to_buffer!(reader, 4), CLASS_FILE_HEADER);

    let minor_version = read_u16!(reader);
    let major_version = MajorVersion::from_u16(read_u16!(reader));
    let constant_pool_count = read_u16!(reader);
    let mut constant_pool: Vec<PoolKind> = Vec::new();
    for i in 0..constant_pool_count - 1 {
        let tag = read_u8!(reader);
        match tag {
            7 => constant_pool.push(PoolKind::Class {
                name_index: read_u16!(reader),
            }),
            9 => constant_pool.push(PoolKind::FieldRef {
                class_index: read_u16!(reader),
                name_and_type_index: read_u16!(reader),
            }),
            10 => constant_pool.push(PoolKind::MethodRef {
                class_index: read_u16!(reader),
                name_and_type_index: read_u16!(reader),
            }),
            11 => constant_pool.push(PoolKind::InterfaceMethodref {
                class_index: read_u16!(reader),
                name_and_type_index: read_u16!(reader),
            }),
            8 => constant_pool.push(PoolKind::String {
                string_index: read_u16!(reader),
            }),
            3 => constant_pool.push(PoolKind::Integer {
                bytes: read_u32!(reader),
            }),
            4 => constant_pool.push(PoolKind::Float {
                bytes: read_u32!(reader),
            }),
            5 => constant_pool.push(PoolKind::Long {
                high_bytes: read_u32!(reader),
                low_bytes: read_u32!(reader),
            }),
            6 => constant_pool.push(PoolKind::Double {
                high_bytes: read_u32!(reader),
                low_bytes: read_u32!(reader),
            }),
            12 => constant_pool.push(PoolKind::NameAndType {
                name_index: read_u16!(reader),
                descriptor_index: read_u16!(reader),
            }),
            1 => {
                let mut buffer = vec![0u8; read_u16!(reader) as usize];
                reader.read_exact(&mut buffer)?;
                constant_pool.push(PoolKind::Utf8 { bytes: buffer })
            }
            15 => constant_pool.push(PoolKind::MethodHandle {
                reference_kind: read_u8!(reader),
                reference_index: read_u16!(reader),
            }),
            16 => constant_pool.push(PoolKind::MethodType {
                descriptor_index: read_u16!(reader),
            }),
            18 => constant_pool.push(PoolKind::InvokeDynamic {
                boostrap_method_attr_index: read_u16!(reader),
                name_and_type_index: read_u16!(reader),
            }),
            _ => println!("{}", tag.to_string()),
        }
    }
    Ok(())
}
