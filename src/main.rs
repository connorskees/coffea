#![allow(dead_code, unused_imports)]
#![deny(missing_debug_implementations, unsafe_code)]

use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead, BufReader, Read};

const TEST_CLASS_FILE_PATH: &str = "test2.class";
const CLASS_FILE_HEADER: [u8; 4] = [0xCA, 0xFE, 0xBA, 0xBE];

type JResult<T> = Result<T, io::Error>;

/// Read `n` bytes as [u8; n]
macro_rules! read_bytes_to_buffer {
    ($reader:expr, $bytes:literal) => {
        if let Some(mut buffer) = Some([0u8; $bytes]) {
            $reader.read_exact(&mut buffer)?;
            // u32::from_be_bytes(buffer).to_be_bytes()
            buffer
        } else {
            unreachable!()
        }
    };
}

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
    Unknown,
}

impl std::default::Default for PoolKind {
    fn default() -> PoolKind {
        PoolKind::Unknown
    }
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

#[derive(Debug, Default)]
pub struct ClassAccessFlags {
    is_public: bool,
    is_final: bool,
    is_super: bool,
    is_interface: bool,
    is_abstract: bool,
    is_synthetic: bool,
    is_annotation: bool,
    is_enum: bool,
}

impl From<u16> for ClassAccessFlags {
    fn from(n: u16) -> Self {
        ClassAccessFlags::from_u16(n)
    }
}

impl ClassAccessFlags {
    const PUBLIC: u16 = 0x0001;
    const FINAL: u16 = 0x0010;
    const SUPER: u16 = 0x0020;
    const INTERFACE: u16 = 0x0200;
    const ABSTRACT: u16 = 0x0400;
    const SYNTHETIC: u16 = 0x1000;
    const ANNOTATION: u16 = 0x2000;
    const ENUM: u16 = 0x4000;

    pub const fn from_u16(n: u16) -> ClassAccessFlags {
        ClassAccessFlags {
            is_public: (n & ClassAccessFlags::PUBLIC) != 0,
            is_final: (n & ClassAccessFlags::FINAL) != 0,
            is_super: (n & ClassAccessFlags::SUPER) != 0,
            is_interface: (n & ClassAccessFlags::INTERFACE) != 0,
            is_abstract: (n & ClassAccessFlags::ABSTRACT) != 0,
            is_synthetic: (n & ClassAccessFlags::SYNTHETIC) != 0,
            is_annotation: (n & ClassAccessFlags::ANNOTATION) != 0,
            is_enum: (n & ClassAccessFlags::ENUM) != 0,
        }
    }
}

#[derive(Debug)]
pub struct FieldAccessFlags {
    is_public: bool,
    is_private: bool,
    is_protected: bool,
    is_static: bool,
    is_final: bool,
    is_volatile: bool,
    is_transient: bool,
    is_synthetic: bool,
    is_enum: bool,
}

impl FieldAccessFlags {
    const PUBLIC: u16 = 0x0001;
    const PRIVATE: u16 = 0x0002;
    const PROTECTED: u16 = 0x0004;
    const STATIC: u16 = 0x0008;
    const FINAL: u16 = 0x0010;
    const VOLATILE: u16 = 0x0040;
    const TRANSIENT: u16 = 0x0080;
    const SYNTHETIC: u16 = 0x1000;
    const ENUM: u16 = 0x4000;

    pub const fn from_u16(n: u16) -> FieldAccessFlags {
        FieldAccessFlags {
            is_public: (n & FieldAccessFlags::PUBLIC) != 0,
            is_private: (n & FieldAccessFlags::PRIVATE) != 0,
            is_protected: (n & FieldAccessFlags::PROTECTED) != 0,
            is_static: (n & FieldAccessFlags::STATIC) != 0,
            is_final: (n & FieldAccessFlags::FINAL) != 0,
            is_volatile: (n & FieldAccessFlags::VOLATILE) != 0,
            is_transient: (n & FieldAccessFlags::TRANSIENT) != 0,
            is_synthetic: (n & FieldAccessFlags::SYNTHETIC) != 0,
            is_enum: (n & FieldAccessFlags::ENUM) != 0,
        }
    }
}

#[derive(Debug, Default)]
pub struct MethodAccessFlags {
    is_public: bool,
    is_private: bool,
    is_protected: bool,
    is_static: bool,
    is_final: bool,
    is_synchronized: bool,
    is_bridge: bool,
    is_var_args: bool,
    is_native: bool,
    is_abstract: bool,
    is_strict: bool,
    is_synthetic: bool,
}

impl MethodAccessFlags {
    const PUBLIC: u16 = 0x0001;
    const PRIVATE: u16 = 0x0002;
    const PROTECTED: u16 = 0x0004;
    const STATIC: u16 = 0x0008;
    const FINAL: u16 = 0x0010;
    const SYNCHRONIZED: u16 = 0x0020;
    const BRIDGE: u16 = 0x0040;
    const VARARGS: u16 = 0x0080;
    const NATIVE: u16 = 0x0100;
    const ABSTRACT: u16 = 0x0400;
    const STRICT: u16 = 0x0800;
    const SYNTHETIC: u16 = 0x1000;

    pub fn from_u16(n: u16) -> MethodAccessFlags {
        MethodAccessFlags {
            is_public: (n & MethodAccessFlags::PUBLIC) != 0,
            is_private: (n & MethodAccessFlags::PRIVATE) != 0,
            is_protected: (n & MethodAccessFlags::PROTECTED) != 0,
            is_static: (n & MethodAccessFlags::STATIC) != 0,
            is_final: (n & MethodAccessFlags::FINAL) != 0,
            is_synchronized: (n & MethodAccessFlags::SYNCHRONIZED) != 0,
            is_bridge: (n & MethodAccessFlags::BRIDGE) != 0,
            is_var_args: (n & MethodAccessFlags::VARARGS) != 0,
            is_native: (n & MethodAccessFlags::NATIVE) != 0,
            is_abstract: (n & MethodAccessFlags::ABSTRACT) != 0,
            is_strict: (n & MethodAccessFlags::STRICT) != 0,
            is_synthetic: (n & MethodAccessFlags::SYNTHETIC) != 0,
        }
    }
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

#[derive(Debug, Default)]
struct MethodInfo {
    access_flags: MethodAccessFlags,
    name_index: u16,
    descriptor_index: u16,
    attributes: Vec<AttributeInfo>,
}

#[derive(Debug, Default)]
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
    Unknown = 0,
}

impl std::default::Default for MajorVersion {
    fn default() -> MajorVersion {
        MajorVersion::Unknown
    }
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

#[derive(Debug, Default)]
struct ClassFile<R: Read + BufRead> {
    pub version: (MajorVersion, u16),
    pub constant_pool: Vec<ConstantPoolInfo>,
    pub access_flags: ClassAccessFlags,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<u16>,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<MethodInfo>,
    pub attributes: Vec<AttributeInfo>,
    reader: R,
}

impl<R: Read + BufRead> ClassFile<R> {
    pub fn from_bufreader(reader: R) -> Result<ClassFile<R>, io::Error> {
        let file: ClassFile<R> = ClassFile {
            version: Default::default(),
            constant_pool: Default::default(),
            access_flags: Default::default(),
            this_class: Default::default(),
            super_class: Default::default(),
            interfaces: Default::default(),
            fields: Default::default(),
            methods: Default::default(),
            attributes: Default::default(),
            reader,
        };
        file.parse()
    }

    fn parse(mut self) -> Result<ClassFile<R>, io::Error> {
        let version = self.read_version()?;
        let constant_pool = self.read_const_pool()?;
        let access_flags = ClassAccessFlags::from_u16(self.read_u16()?);
        let this_class = self.read_u16()?;
        let super_class = self.read_u16()?;
        let interfaces = self.read_interfaces()?;
        let fields = self.read_fields()?;
        let methods = self.read_methods()?;

        // Ok(ClassFile {
        //     version, constant_pool, access_flags, this_class, super_class, interfaces, fields, reader: self.reader
        // })
        unimplemented!()
    }

    fn read_version(&mut self) -> JResult<(MajorVersion, u16)> {
        let minor_version = self.read_u16()?;
        let major_version = MajorVersion::from_u16(self.read_u16()?);

        Ok((major_version, minor_version))
    }

    fn read_const_pool(&mut self) -> JResult<Vec<PoolKind>> {
        let constant_pool_count = self.read_u16()?;
        let mut constant_pool: Vec<PoolKind> = Vec::new();
        let mut i = 1;
        while i <= constant_pool_count - 1 {
            let tag = self.read_u8()?;
            constant_pool.push(match tag {
                1 => {
                    let mut buffer = vec![0u8; self.read_u16()? as usize];
                    self.reader.read_exact(&mut buffer)?;
                    PoolKind::Utf8 { bytes: buffer }
                }
                3 => PoolKind::integer(self.read_u32()?),
                4 => PoolKind::float(self.read_u32()?),
                5 => {
                    // doubles and longs count as 2 spots
                    i += 1;
                    PoolKind::long(self.read_u32()?, self.read_u32()?)
                }
                6 => {
                    i += 1;
                    PoolKind::double(self.read_u32()?, self.read_u32()?)
                }
                7 => PoolKind::class(self.read_u16()?),
                8 => PoolKind::string(self.read_u16()?),
                9 => PoolKind::field_ref(self.read_u16()?, self.read_u16()?),
                10 => PoolKind::method_ref(self.read_u16()?, self.read_u16()?),
                11 => PoolKind::interface_method_ref(self.read_u16()?, self.read_u16()?),
                12 => PoolKind::name_and_type(self.read_u16()?, self.read_u16()?),
                15 => PoolKind::method_handle(self.read_u8()?, self.read_u16()?),
                16 => PoolKind::method_type(self.read_u16()?),
                18 => PoolKind::invoke_dynamic(self.read_u16()?, self.read_u16()?),
                _ => unimplemented!("unrecognized tag kind"),
            });
            i += 1;
        }
        Ok(constant_pool)
    }

    fn read_interfaces(&mut self) -> JResult<Vec<u16>> {
        let interface_count = self.read_u16()?;
        let mut interfaces = Vec::new();

        for _ in 0..interface_count {
            interfaces.push(self.read_u16()?);
        }
        Ok(interfaces)
    }

    fn read_fields(&mut self) -> JResult<Vec<FieldInfo>> {
        let field_count = self.read_u16()?;
        let mut fields = Vec::new();

        for _ in 0..field_count {
            let access_flags = FieldAccessFlags::from_u16(self.read_u16()?);
            let name_index = self.read_u16()?;
            let descriptor_index = self.read_u16()?;
            let attributes_count = self.read_u16()?;
            let attributes = self.read_attributes(attributes_count)?;

            fields.push(FieldInfo {
                access_flags,
                name_index,
                descriptor_index,
                attribute_info: attributes,
            })
        }
        Ok(fields)
    }

    fn read_attribute(&mut self) -> JResult<AttributeInfo> {
        let attribute_name_index = self.read_u16()?;
        let attribute_length = self.read_u32()?;

        let mut info = vec![0u8; attribute_length as usize];
        self.reader.read_exact(&mut info)?;

        Ok(AttributeInfo {
            attribute_name_index,
            attribute_length,
            info,
        })
    }

    fn read_attributes(&mut self, count: u16) -> JResult<Vec<AttributeInfo>> {
        let mut attributes = Vec::new();
        for _ in 0..count {
            attributes.push(self.read_attribute()?);
        }
        Ok(attributes)
    }

    fn read_methods(&mut self) -> JResult<Vec<MethodInfo>> {
        let method_count = self.read_u16()?;
        let mut methods = Vec::new();
        for _ in 0..method_count {
            let access_flags = MethodAccessFlags::from_u16(self.read_u16()?);
            let name_index = self.read_u16()?;
            let descriptor_index = self.read_u16()?;
            let attributes_count = self.read_u16()?;
            let attributes = self.read_attributes(attributes_count)?;
            methods.push(MethodInfo { access_flags, name_index, descriptor_index, attributes });
        }

        Ok(methods)
    }
}

impl<R: Read + BufRead> ClassFile<R> {
    /// Read a single byte as a u8
    fn read_u8(&mut self) -> Result<u8, io::Error> {
        let mut buffer = [0u8];
        self.reader.read_exact(&mut buffer)?;
        Ok(u8::from_be_bytes(buffer))
    }

    /// Read 2 bytes as a u16
    fn read_u16(&mut self) -> Result<u16, io::Error> {
        let mut buffer = [0u8; 2];
        self.reader.read_exact(&mut buffer)?;
        Ok(u16::from_be_bytes(buffer))
    }

    /// Read 4 bytes as a u32

    fn read_u32(&mut self) -> Result<u32, io::Error> {
        let mut buffer = [0u8; 4];
        self.reader.read_exact(&mut buffer)?;
        Ok(u32::from_be_bytes(buffer))
    }
}

fn main() -> io::Result<()> {
    let mut reader = BufReader::new(File::open(TEST_CLASS_FILE_PATH)?);
    assert_eq!(read_bytes_to_buffer!(reader, 4), CLASS_FILE_HEADER);

    Ok(())
}
