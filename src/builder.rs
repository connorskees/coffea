use std::io::{BufRead, Read};

use crate::{
    attributes::{Attribute, ExceptionTableEntry, LocalVariableTableEntry, *},
    code::Code,
    errors::{JResult, ParseError},
    methods::{MethodAccessFlags, MethodDescriptor, MethodInfo},
    pool::PoolKind,
    {ClassAccessFlags, ClassFile},
};
pub use crate::{
    common::Type,
    fields::{FieldAccessFlags, FieldInfo},
    version::MajorVersion,
};

const CLASS_FILE_HEADER: [u8; 4] = [0xCA, 0xFE, 0xBA, 0xBE];

/// Read `n` bytes as [u8; n]
/// This is a hack until const generics
macro_rules! read_bytes_to_buffer {
    ($reader:expr, $bytes:literal) => {
        if let Some(mut buffer) = Some([0_u8; $bytes]) {
            $reader.read_exact(&mut buffer)?;
            // u32::from_be_bytes(buffer).to_be_bytes()
            buffer
        } else {
            unreachable!()
        }
    };
}

pub(crate) struct ClassFileBuilder<R: Read + BufRead> {
    pub(crate) reader: R,
    pub(crate) const_pool: Vec<PoolKind>,
}

/// High level methods for parsing class files
impl<R: Read + BufRead> ClassFileBuilder<R> {
    pub(crate) fn parse(mut self) -> JResult<ClassFile> {
        let header = read_bytes_to_buffer!(self.reader, 4);
        assert_eq!(header, CLASS_FILE_HEADER);
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
            const_pool: self.const_pool,
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
        let const_pool_count = self.read_u16()?;
        let mut const_pool: Vec<PoolKind> = Vec::new();
        let mut i = 1;
        let mut push_twice = false;
        while i < const_pool_count {
            let tag = self.read_u8()?;
            const_pool.push(match tag {
                1 => {
                    let mut buffer = vec![0_u8; self.read_u16()? as usize];
                    self.reader.read_exact(&mut buffer)?;
                    PoolKind::utf8(&buffer)
                }
                3 => PoolKind::integer(self.read_u32()?),
                4 => PoolKind::float(self.read_u32()?),
                5 => {
                    // doubles and longs count as 2 spots
                    i += 1;
                    push_twice = true;
                    PoolKind::long(i64::from_be_bytes(read_bytes_to_buffer!(self.reader, 8)))
                }
                6 => {
                    i += 1;
                    push_twice = true;
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
            if push_twice {
                const_pool.push(PoolKind::Long(0));
                push_twice = false;
            }
            i += 1;
        }
        Ok(const_pool)
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

        Ok(
            match self.const_pool[usize::from(attribute_name_index - 1)] {
                PoolKind::Utf8(ref s) => match s.as_str() {
                    "ConstantValue" => self.parse_attr_constant_value()?,
                    "Code" => self.parse_attr_code()?,
                    "StackMapTable" => self.parse_attr_stack_map_table()?,
                    "Exceptions" => self.parse_attr_exceptions()?,
                    "InnerClasses" => self.parse_attr_inner_classes()?,
                    "EnclosingMethod" => self.parse_attr_enclosing_method()?,
                    "Synthetic" => Attribute::Synthetic,
                    "MethodSignature" => self.parse_attr_signature()?,
                    "SourceFile" => self.parse_attr_source_file()?,
                    "SourceDebugExtension" => {
                        self.parse_attr_source_debug_extension(attribute_length)?
                    }
                    "LineNumberTable" => self.parse_attr_line_number_table()?,
                    "LocalVariableTable" => self.parse_attr_local_variable_table()?,
                    "LocalVariableTypeTable" => self.parse_attr_local_variable_type_table()?,
                    "Deprecated" => Attribute::Deprecated,
                    "RuntimeVisibleAnnotations" => self.parse_attr_runtime_visible_annotations()?,
                    "RuntimeInvisibleAnnotations" => {
                        self.parse_attr_runtime_invisible_annotations()?
                    }
                    "RuntimeVisibleParameterAnnotations" => {
                        self.parse_attr_runtime_visible_parameter_annotations()?
                    }
                    "RuntimeInvisibleParameterAnnotations" => {
                        self.parse_attr_runtime_invisible_parameter_annotations()?
                    }
                    "AnnotationDefault" => self.parse_attr_annotation_default()?,
                    "BootstrapMethods" => self.parse_attr_bootstrap_methods()?,
                    _ => {
                        let mut info = vec![0_u8; attribute_length as usize];
                        self.reader.read_exact(&mut info)?;
                        Attribute::Other { info }
                    }
                },
                _ => unimplemented!("TODO: handle non-PoolKind::Utf8 in `read_attribute()`"),
            },
        )
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
            let name = if let PoolKind::Utf8(s) = &self.const_pool[usize::from(name_index - 1)] {
                s.clone()
            } else {
                return Err(ParseError::IndexError(line!()));
            };
            let descriptor_index = self.read_u16()?;
            let MethodDescriptor { args, return_type } =
                if let PoolKind::Utf8(s) = &self.const_pool[usize::from(descriptor_index - 1)] {
                    MethodDescriptor::new(s)
                } else {
                    return Err(ParseError::IndexError(line!()));
                };
            let attributes_count = self.read_u16()?;
            let attributes = self.read_attributes(attributes_count)?;
            methods.push(MethodInfo {
                access_flags,
                name,
                args,
                return_type,
                attributes,
            });
        }

        Ok(methods)
    }
}

/// Methods for parsing attributes
impl<R: Read + BufRead> ClassFileBuilder<R> {
    fn parse_attr_constant_value(&mut self) -> JResult<Attribute> {
        Ok(Attribute::ConstantValue {
            const_value_index: self.read_u16()?,
        })
    }

    fn parse_attr_code(&mut self) -> JResult<Attribute> {
        let max_stack = self.read_u16()?;
        let max_locals = self.read_u16()?;

        let code_length = self.read_u32()?;
        let mut code = vec![0_u8; code_length as usize];
        self.reader.read_exact(&mut code)?;

        let exception_table_length = self.read_u16()?;
        let exception_table = self.read_exception_table(exception_table_length)?;

        let attributes_count = self.read_u16()?;
        let attribute_info = self.read_attributes(attributes_count)?;

        Ok(Attribute::Code(Code {
            max_stack,
            max_locals,
            code,
            exception_table,
            attribute_info,
        }))
    }

    fn read_exception_table_entry(&mut self) -> JResult<ExceptionTableEntry> {
        let start = self.read_u16()?;
        let end = self.read_u16()?;
        let handler = self.read_u16()?;
        let catch_type = self.read_u16()?;

        Ok(ExceptionTableEntry {
            start,
            end,
            handler,
            catch_type,
        })
    }

    fn read_exception_table(&mut self, len: u16) -> JResult<Vec<ExceptionTableEntry>> {
        let mut entries = Vec::new();
        for _ in 0..len {
            entries.push(self.read_exception_table_entry()?);
        }
        Ok(entries)
    }

    fn parse_attr_stack_map_table(&mut self) -> JResult<Attribute> {
        let number_of_entries = self.read_u16()?;
        let mut table: Vec<FrameType> = Vec::new();
        for _ in 0..number_of_entries {
            let tag = self.read_u8()?;
            table.push(match tag {
                0..=63 => FrameType::Same {
                    offset_delta: u16::from(tag),
                },
                64..=127 => FrameType::SameLocals1StackItem {
                    offset_delta: u16::from(tag - 64),
                    stack: self.read_verification_type_info()?,
                },
                128..=246 => unimplemented!("TODO: reserved tags"),
                247 => FrameType::SameLocals1StackItemExtended {
                    offset_delta: self.read_u16()?,
                    stack: self.read_verification_type_info()?,
                },
                248..=250 => FrameType::Chop {
                    k: 251 - tag,
                    offset_delta: self.read_u16()?,
                },
                251 => FrameType::SameExtended {
                    offset_delta: self.read_u16()?,
                },
                252..=254 => {
                    let offset_delta = self.read_u16()?;
                    let mut locals = Vec::new();
                    for _ in 0..(tag - 251) {
                        locals.push(self.read_verification_type_info()?);
                    }
                    FrameType::Append {
                        offset_delta,
                        locals,
                    }
                }
                255 => {
                    let offset_delta = self.read_u16()?;
                    let number_of_locals = self.read_u16()?;
                    let mut locals = Vec::new();
                    for _ in 0..number_of_locals {
                        locals.push(self.read_verification_type_info()?);
                    }
                    let number_of_stack_items = self.read_u16()?;
                    let mut stack = Vec::new();
                    for _ in 0..number_of_stack_items {
                        stack.push(self.read_verification_type_info()?);
                    }
                    FrameType::Full {
                        offset_delta,
                        locals,
                        stack,
                    }
                }
            });
        }
        Ok(Attribute::StackMapTable(table))
    }

    fn read_verification_type_info(&mut self) -> JResult<VerificationTypeInfo> {
        let tag = self.read_u8()?;
        Ok(match tag {
            0 => VerificationTypeInfo::Top,
            1 => VerificationTypeInfo::Integer,
            2 => VerificationTypeInfo::Float,
            3 => VerificationTypeInfo::Double,
            4 => VerificationTypeInfo::Long,
            5 => VerificationTypeInfo::Null,
            6 => VerificationTypeInfo::UninitializedThis,
            7 => VerificationTypeInfo::Object(self.read_u16()?),
            8 => VerificationTypeInfo::UninitializedVar {
                offset: self.read_u16()?,
            },
            _ => unimplemented!("TODO: invalid verification type info tag (>8)"),
        })
    }

    fn parse_attr_exceptions(&mut self) -> JResult<Attribute> {
        let number_of_exceptions = self.read_u16()?;
        let mut exceptions: Vec<u16> = Vec::with_capacity(usize::from(number_of_exceptions));
        for _ in 0..number_of_exceptions {
            exceptions.push(self.read_u16()?);
        }
        Ok(Attribute::Exceptions(exceptions))
    }

    fn parse_attr_inner_classes(&mut self) -> JResult<Attribute> {
        let number_of_classes = self.read_u16()?;
        let mut classes = Vec::new();
        for _ in 0..number_of_classes {
            classes.push(self.read_inner_class()?);
        }
        Ok(Attribute::InnerClasses(classes))
    }

    fn read_inner_class(&mut self) -> JResult<ClassInfo> {
        let inner_class_info_index = self.read_u16()?;
        let outer_class_info_index = self.read_u16()?;
        let inner_name_index = self.read_u16()?;
        let inner_class_access_flags = InnerClassFlags::from_u16(self.read_u16()?);
        Ok(ClassInfo {
            inner_class_info_index,
            outer_class_info_index,
            inner_name_index,
            inner_class_access_flags,
        })
    }

    fn parse_attr_enclosing_method(&mut self) -> JResult<Attribute> {
        let class_index = self.read_u16()?;
        let method_index = self.read_u16()?;
        Ok(Attribute::EnclosingMethod {
            class_index,
            method_index,
        })
    }

    fn parse_attr_signature(&mut self) -> JResult<Attribute> {
        Ok(Attribute::Signature(self.read_u16()?))
    }

    fn parse_attr_source_file(&mut self) -> JResult<Attribute> {
        Ok(Attribute::SourceFile(self.read_u16()?))
    }

    fn parse_attr_source_debug_extension(&mut self, attribute_length: u32) -> JResult<Attribute> {
        let mut debug_extension = vec![0_u8; attribute_length as usize];
        self.reader.read_exact(&mut debug_extension)?;
        Ok(Attribute::SourceDebugExtension(debug_extension))
    }

    fn parse_attr_line_number_table(&mut self) -> JResult<Attribute> {
        let line_number_table_length = self.read_u16()?;
        let mut line_number_table = Vec::new();
        for _ in 0..line_number_table_length {
            let start = self.read_u16()?;
            let line_number = self.read_u16()?;
            line_number_table.push(LineNumberTableEntry { start, line_number });
        }
        Ok(Attribute::LineNumberTable(line_number_table))
    }

    fn parse_attr_local_variable_table(&mut self) -> JResult<Attribute> {
        let local_variable_table_length = self.read_u16()?;
        let mut entries = Vec::new();
        for _ in 0..local_variable_table_length {
            entries.push(self.read_local_variable_table_entry()?);
        }

        Ok(Attribute::LocalVariableTable(entries))
    }

    fn read_local_variable_table_entry(&mut self) -> JResult<LocalVariableTableEntry> {
        let start = self.read_u16()?;
        let length = self.read_u16()?;
        let name_index = self.read_u16()?;
        let descriptor_index = self.read_u16()?;
        let index = self.read_u16()?;
        Ok(LocalVariableTableEntry {
            start,
            length,
            name_index,
            descriptor_index,
            index,
        })
    }

    fn parse_attr_local_variable_type_table(&mut self) -> JResult<Attribute> {
        let local_variable_type_table_length = self.read_u16()?;
        let mut entries = Vec::new();
        for _ in 0..local_variable_type_table_length {
            entries.push(self.read_local_variable_table_entry()?);
        }

        Ok(Attribute::LocalVariableTypeTable(entries))
    }

    fn read_element_value(&mut self) -> JResult<ElementValue> {
        let tag = self.read_u8()?;
        let const_value_index = self.read_u16()?;
        let type_name_index = self.read_u16()?;
        let const_name_index = self.read_u16()?;
        let enum_const_value = EnumConstValue {
            type_name_index,
            const_name_index,
        };
        let class_info_index = self.read_u16()?;
        let annotation_value = self.read_annotation()?;

        let mut values: Vec<ElementValue> = Vec::new();
        let num_values = self.read_u16()?;
        for _ in 0..num_values {
            values.push(self.read_element_value()?)
        }

        Ok(ElementValue {
            tag,
            const_value_index,
            enum_const_value,
            class_info_index,
            annotation_value,
            values,
        })
    }

    fn read_annotation(&mut self) -> JResult<Annotation> {
        let type_index = self.read_u16()?;
        let num_element_value_pairs = self.read_u16()?;
        let mut element_value_pairs: Vec<ElementValuePair> = Vec::new();
        for _ in 0..num_element_value_pairs {
            let element_name_index = self.read_u16()?;
            let element_value = self.read_element_value()?;
            element_value_pairs.push(ElementValuePair {
                element_name_index,
                element_value,
            });
        }
        Ok(Annotation {
            type_index,
            element_value_pairs,
        })
    }

    fn parse_attr_runtime_visible_annotations(&mut self) -> JResult<Attribute> {
        let num_annotations = self.read_u16()?;
        let mut annotations: Vec<Annotation> = Vec::new();
        for _ in 0..num_annotations {
            annotations.push(self.read_annotation()?);
        }

        Ok(Attribute::RuntimeVisibleAnnotations(annotations))
    }
    fn parse_attr_runtime_invisible_annotations(&mut self) -> JResult<Attribute> {
        let num_annotations = self.read_u16()?;
        let mut annotations: Vec<Annotation> = Vec::new();
        for _ in 0..num_annotations {
            annotations.push(self.read_annotation()?);
        }

        Ok(Attribute::RuntimeInvisibleAnnotations(annotations))
    }
    fn parse_attr_runtime_visible_parameter_annotations(&mut self) -> JResult<Attribute> {
        let num_parameters = self.read_u8()?;
        let mut parameter_annotations: Vec<Vec<Annotation>> = Vec::new();
        for _ in 0..num_parameters {
            let num_annotations = self.read_u16()?;
            let mut annotations: Vec<Annotation> = Vec::new();
            for _ in 0..num_annotations {
                annotations.push(self.read_annotation()?);
            }
            parameter_annotations.push(annotations);
        }

        Ok(Attribute::RuntimeVisibleParameterAnnotations(
            parameter_annotations,
        ))
    }
    fn parse_attr_runtime_invisible_parameter_annotations(&mut self) -> JResult<Attribute> {
        let num_parameters = self.read_u8()?;
        let mut parameter_annotations: Vec<Vec<Annotation>> = Vec::new();
        for _ in 0..num_parameters {
            let num_annotations = self.read_u16()?;
            let mut annotations: Vec<Annotation> = Vec::new();
            for _ in 0..num_annotations {
                annotations.push(self.read_annotation()?);
            }
            parameter_annotations.push(annotations);
        }

        Ok(Attribute::RuntimeInvisibleParameterAnnotations(
            parameter_annotations,
        ))
    }
    fn parse_attr_annotation_default(&mut self) -> JResult<Attribute> {
        let element_value = self.read_element_value()?;
        Ok(Attribute::AnnotationDefault(element_value))
    }

    fn parse_attr_bootstrap_methods(&mut self) -> JResult<Attribute> {
        let num_bootstrap_methods = self.read_u16()?;
        let mut entries = Vec::new();
        for _ in 0..num_bootstrap_methods {
            entries.push(self.read_bootstrap_method()?);
        }
        Ok(Attribute::BootstrapMethods(entries))
    }

    fn read_bootstrap_method(&mut self) -> JResult<BootstrapMethod> {
        let bootstrap_method_ref = self.read_u16()?;
        let num_bootstrap_arguments = self.read_u16()?;
        let mut bootstrap_arguments = Vec::with_capacity(usize::from(num_bootstrap_arguments));
        for _ in 0..num_bootstrap_arguments {
            bootstrap_arguments.push(self.read_u16()?);
        }
        Ok(BootstrapMethod {
            bootstrap_method_ref,
            bootstrap_arguments,
        })
    }
}

/// Helper methods for reading common numbers of bytes
impl<R: Read + BufRead> ClassFileBuilder<R> {
    /// Read a single byte as a u8
    fn read_u8(&mut self) -> JResult<u8> {
        let mut buffer = [0_u8];
        self.reader.read_exact(&mut buffer)?;
        Ok(u8::from_be_bytes(buffer))
    }

    /// Read 2 bytes as a u16
    fn read_u16(&mut self) -> JResult<u16> {
        let mut buffer = [0_u8; 2];
        self.reader.read_exact(&mut buffer)?;
        Ok(u16::from_be_bytes(buffer))
    }

    /// Read 4 bytes as a u32
    fn read_u32(&mut self) -> JResult<u32> {
        let mut buffer = [0_u8; 4];
        self.reader.read_exact(&mut buffer)?;
        Ok(u32::from_be_bytes(buffer))
    }
}
