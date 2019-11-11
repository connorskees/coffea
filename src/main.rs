#![allow(dead_code, unused_imports)]
#![deny(missing_debug_implementations, unsafe_code)]

use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead, BufReader, Read};

const TEST_CLASS_FILE_PATH: &str = "test2.class";
const CLASS_FILE_HEADER: [u8; 4] = [0xCA, 0xFE, 0xBA, 0xBE];

#[derive(Debug)]
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
    InterfaceMethodRef {
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

impl PoolKind {
    fn class(name_index: u16) -> PoolKind {
        PoolKind::Class { name_index }
    }

    fn field_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::FieldRef {
            class_index,
            name_and_type_index,
        }
    }

    fn method_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::MethodRef {
            class_index,
            name_and_type_index,
        }
    }

    fn interface_method_ref(class_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::InterfaceMethodRef {
            class_index,
            name_and_type_index,
        }
    }

    fn string(string_index: u16) -> PoolKind {
        PoolKind::String { string_index }
    }

    fn integer(bytes: u32) -> PoolKind {
        PoolKind::Integer { bytes }
    }

    fn float(bytes: u32) -> PoolKind {
        PoolKind::Float { bytes }
    }

    fn long(high_bytes: u32, low_bytes: u32) -> PoolKind {
        PoolKind::Long {
            high_bytes,
            low_bytes,
        }
    }

    fn double(high_bytes: u32, low_bytes: u32) -> PoolKind {
        PoolKind::Double {
            high_bytes,
            low_bytes,
        }
    }

    fn name_and_type(name_index: u16, descriptor_index: u16) -> PoolKind {
        PoolKind::NameAndType {
            name_index,
            descriptor_index,
        }
    }

    fn utf8(bytes: Vec<u8>) -> PoolKind {
        PoolKind::Utf8 { bytes }
    }

    fn method_handle(reference_kind: u8, reference_index: u16) -> PoolKind {
        PoolKind::MethodHandle {
            reference_kind,
            reference_index,
        }
    }

    fn method_type(descriptor_index: u16) -> PoolKind {
        PoolKind::MethodType { descriptor_index }
    }

    fn invoke_dynamic(boostrap_method_attr_index: u16, name_and_type_index: u16) -> PoolKind {
        PoolKind::InvokeDynamic {
            boostrap_method_attr_index,
            name_and_type_index,
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
struct AttributeInfo {
    attribute_name_index: u16,
    attribute_length: u32,
    info: Vec<u8>,
}

#[derive(Debug)]
struct FieldInfo {
    access_flags: FieldAccessFlags,
    name_index: u16,
    descriptor_index: u16,
    attribute_info: Vec<AttributeInfo>,
}

#[derive(Debug)]
struct MethodInfo {
    access_flags: u16,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<AttributeInfo>,
}

#[derive(Debug)]
struct ConstantPoolInfo {
    pool_kind: PoolKind,
    bytes: Vec<u8>,
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    println!("constant_pool_count {}", constant_pool_count.to_string());
    let mut constant_pool: Vec<PoolKind> = Vec::new();
    let mut i = 1;
    while i <= constant_pool_count-1 {
        let tag = read_u8!(reader);
        println!("tag {}", tag.to_string());
        constant_pool.push(match tag {
            1 => {
                let length = read_u16!(reader);
                let mut buffer = vec![0u8; length as usize];
                reader.read_exact(&mut buffer)?;
                println!(
                    "str {}, i{}",
                    std::str::from_utf8(&buffer).unwrap(),
                    i.to_string()
                );
                PoolKind::Utf8 { bytes: buffer }
            }
            3 => PoolKind::integer(read_u32!(reader)),
            4 => PoolKind::float(read_u32!(reader)),
            5 => {
                // doubles and longs count as 2 spots
                i += 1;
                PoolKind::long(read_u32!(reader), read_u32!(reader))
            },
            6 => {
                i += 1;
                PoolKind::double(read_u32!(reader), read_u32!(reader))
            },
            7 => PoolKind::class(read_u16!(reader)),
            8 => PoolKind::string(read_u16!(reader)),
            9 => PoolKind::field_ref(read_u16!(reader), read_u16!(reader)),
            10 => PoolKind::method_ref(read_u16!(reader), read_u16!(reader)),
            11 => PoolKind::interface_method_ref(read_u16!(reader), read_u16!(reader)),
            12 => PoolKind::name_and_type(read_u16!(reader), read_u16!(reader)),
            15 => PoolKind::method_handle(read_u8!(reader), read_u16!(reader)),
            16 => PoolKind::method_type(read_u16!(reader)),
            18 => PoolKind::invoke_dynamic(read_u16!(reader), read_u16!(reader)),
            _ => unimplemented!("unrecognized tag kind"),
        });
        i += 1;
    }

    let access_flags = read_u16!(reader);
    let this_class = read_u16!(reader);
    let super_class = read_u16!(reader);

    Ok(())
}
