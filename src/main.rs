#![allow(dead_code, unused_imports)]
#![deny(missing_debug_implementations, unsafe_code)]

use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufRead, BufReader, Read};

use crate::attributes::Attribute;
pub use crate::fields::{FieldAccessFlags, FieldInfo};
use crate::methods::{MethodAccessFlags, MethodInfo};
pub use crate::pool::PoolKind;
pub use crate::version::MajorVersion;

const TEST_CLASS_FILE_PATH: &str = "test3.class";
const CLASS_FILE_HEADER: [u8; 4] = [0xCA, 0xFE, 0xBA, 0xBE];

type JResult<T> = Result<T, io::Error>;

pub mod attributes;
mod fields;
pub mod methods;
mod pool;
mod version;

/// Read `n` bytes as [u8; n]
/// This is a hack until const generics
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
pub struct ClassFile {
    pub version: (MajorVersion, u16),
    pub constant_pool: Vec<PoolKind>,
    pub access_flags: ClassAccessFlags,
    pub this_class: u16,
    pub super_class: u16,
    pub interfaces: Vec<u16>,
    pub fields: Vec<FieldInfo>,
    pub methods: Vec<MethodInfo>,
    pub attributes: Vec<Attribute>,
}

impl ClassFile {
    pub fn from_bufreader<R: Read + BufRead>(reader: R) -> JResult<ClassFile> {
        ClassFileBuilder {
            reader,
            const_pool: Vec::new(),
        }
        .parse()
    }

    pub fn from_path<P: AsRef<std::path::Path>>(p: P) -> JResult<ClassFile> {
        let buffer = BufReader::new(File::open(p).unwrap());
        ClassFile::from_bufreader(buffer)
    }
}

struct ClassFileBuilder<R: Read + BufRead> {
    reader: R,
    const_pool: Vec<PoolKind>,
}

impl<R: Read + BufRead> ClassFileBuilder<R> {
    fn parse(mut self) -> JResult<ClassFile> {
        assert_eq!(read_bytes_to_buffer!(self.reader, 4), CLASS_FILE_HEADER);
        let version = self.read_version()?;
        self.const_pool = self.read_const_pool()?;
        let access_flags = ClassAccessFlags::from_u16(self.read_u16()?);
        let this_class = self.read_u16()?;
        let super_class = self.read_u16()?;
        let interfaces = self.read_interfaces()?;
        let fields = self.read_fields()?;
        let methods = self.read_methods()?;
        let attributes_count = self.read_u16()?;
        let attributes = self.read_attributes(attributes_count)?;

        Ok(ClassFile {
            version,
            constant_pool: self.const_pool,
            access_flags,
            this_class,
            super_class,
            interfaces,
            fields,
            methods,
            attributes,
        })
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
                    PoolKind::utf8(buffer)
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

    fn read_attribute(&mut self) -> JResult<Attribute> {
        let attribute_name_index = self.read_u16()?;
        let attribute_length = self.read_u32()?;

        let mut info = vec![0u8; attribute_length as usize];
        self.reader.read_exact(&mut info)?;

        Ok(match self.const_pool[usize::from(attribute_name_index)] {
            PoolKind::Utf8(ref s) => match s.as_str() {
                "ConstantValue" => self.read_attr_constant_value()?,
                "Code" => self.read_attr_code()?,
                "StackMapTable" => self.read_attr_stack_map_table()?,
                "Exceptions" => self.read_attr_exceptions()?,
                "InnerClasses" => self.read_attr_inner_classes()?,
                "EnclosingMethod" => self.read_attr_enclosing_method()?,
                "Synthetic" => self.read_attr_synthetic()?,
                "Signature" => self.read_attr_signature()?,
                "SourceFile" => self.read_attr_source_file()?,
                "SourceDebugExtension" => self.read_attr_source_debug_extension()?,
                "LineNumberTable" => self.read_attr_line_number_table()?,
                "LocalVariableTable" => self.read_attr_local_variable_table()?,
                "LocalVariableTypeTable" => self.read_attr_local_variable_type_table()?,
                "Deprecated" => Attribute::Deprecated,
                "RuntimeVisibleAnnotations" => self.read_attr_runtime_visible_annotations()?,
                "RuntimeInvisibleAnnotations" => self.read_attr_runtime_invisible_annotations()?,
                "RuntimeVisibleParameterAnnotations" => self.read_attr_runtime_visible_parameter_annotations()?,
                "RuntimeInvisibleParameterAnnotations" => self.read_attr_runtime_invisible_parameter_annotations()?,
                "AnnotationDefault" => self.read_attr_annotation_default()?,
                "BootstrapMethods" => self.read_attr_bootstrap_methods()?,
                "Other" => self.read_attr_other()?,
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        })
    }

    fn read_attributes(&mut self, count: u16) -> JResult<Vec<Attribute>> {
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
            methods.push(MethodInfo {
                access_flags,
                name_index,
                descriptor_index,
                attributes,
            });
        }

        Ok(methods)
    }
}

impl<R: Read + BufRead> ClassFileBuilder<R> {
    fn read_attr_constant_value(&mut self) -> JResult<Attribute> {
        Ok(Attribute::ConstantValue { const_value_index: self.read_u16()? })
    }
    fn read_attr_code(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_stack_map_table(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_exceptions(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_inner_classes(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_enclosing_method(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_synthetic(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_signature(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_source_file(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_source_debug_extension(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_line_number_table(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_local_variable_table(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_local_variable_type_table(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_runtime_visible_annotations(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_runtime_invisible_annotations(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_runtime_visible_parameter_annotations(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_runtime_invisible_parameter_annotations(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_annotation_default(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_bootstrap_methods(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
    fn read_attr_other(&mut self) -> JResult<Attribute> {
        unimplemented!()
    }
}


impl<R: Read + BufRead> ClassFileBuilder<R> {
    /// Read a single byte as a u8
    fn read_u8(&mut self) -> JResult<u8> {
        let mut buffer = [0u8];
        self.reader.read_exact(&mut buffer)?;
        Ok(u8::from_be_bytes(buffer))
    }

    /// Read 2 bytes as a u16
    fn read_u16(&mut self) -> JResult<u16> {
        let mut buffer = [0u8; 2];
        self.reader.read_exact(&mut buffer)?;
        Ok(u16::from_be_bytes(buffer))
    }

    /// Read 4 bytes as a u32
    fn read_u32(&mut self) -> JResult<u32> {
        let mut buffer = [0u8; 4];
        self.reader.read_exact(&mut buffer)?;
        Ok(u32::from_be_bytes(buffer))
    }
}

fn main() -> io::Result<()> {
    let reader = BufReader::new(File::open(TEST_CLASS_FILE_PATH)?);
    let file = ClassFile::from_bufreader(reader)?;

    // for a in file.attributes {
    // dbg!(&file.constant_pool[(a.attribute_name_index-1) as usize]);
    // }
    // let index = .name_index;
    dbg!(&file.methods[0]);
    // dbg!(file.this_class, file.super_class);
    dbg!(&file.constant_pool[(8) as usize]);

    Ok(())
}
