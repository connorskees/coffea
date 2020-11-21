use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader, Read, Write},
};

use crate::{
    ast::{AstVisitor, AST},
    attributes::{Attribute, BootstrapMethod},
    builder::ClassFileBuilder,
    common::{double_to_f64, float_to_f32, Indent, Type},
    errors::{JResult, ParseError},
    fields::{FieldDescriptor, FieldInfo},
    methods::{MethodDescriptor, MethodInfo},
    pool::PoolKind,
    version::MajorVersion,
    Codegen, StackEntry,
};

struct ClassFileVisitor<W: Write> {
    class_file: ClassFile,
    buf: W,
    indent: Indent,
    /// map from name to value of init values of class fields
    fields: HashMap<String, AST>,
}

impl<W: Write> ClassFileVisitor<W> {
    /// Visit function `<init>`, necessary for setup of class
    /// fields, among other things. `<init>` should always be
    /// visited first
    fn visit_init(&mut self, init: &MethodInfo) -> JResult<()> {
        let tokens = init.code().unwrap().lex();

        let mut fields = HashMap::new();

        let mut local_variables = HashMap::new();

        local_variables.insert(
            0,
            StackEntry::Ident(
                "this".to_owned(),
                Type::ClassName(self.class_file.class_name()?.to_owned()),
            ),
        );

        Codegen {
            class: &mut self.class_file,
            stack: Vec::new(),
            local_variables,
            tokens,
            ast: Vec::new(),
            current_pos: 0,
            fields: &mut fields,
            inside_init: true,
        }
        .codegen()?;

        self.fields = fields;

        Ok(())
    }

    fn visit_class(&mut self) -> JResult<()> {
        self.buf
            .write_all(self.class_file.class_signature()?.as_bytes())?;

        self.indent.increase();

        for method in self.class_file.methods.clone() {
            if method.name == "<init>" {
                self.visit_init(&method)?;

                for field in self.class_file.fields.clone() {
                    self.visit_field(field)?;
                }
                continue;
            }

            if method.name == "<cinit>" {
                continue;
            }

            self.indent.write(&mut self.buf)?;
            self.indent.increase();
            self.buf.write_all(method.signature().as_bytes())?;

            self.visit_method_body(&method)?;

            self.indent.decrease();
            self.indent.write(&mut self.buf)?;
            self.buf.write_all(b"}\n")?;
        }

        self.buf.write_all(b"}\n")?;

        Ok(())
    }

    // todo: don't need by value
    fn visit_field(&mut self, field: FieldInfo) -> JResult<()> {
        self.indent.write(&mut self.buf)?;

        field.access_flags.write(&mut self.buf)?;

        let ty = FieldDescriptor::new(&self.class_file.utf_from_index(field.descriptor_index)?).ty;
        let name = self.class_file.utf_from_index(field.name_index)?;

        write!(self.buf, "{} {} = ", ty, name)?;
        if field.access_flags.is_static {
            let mut const_index = None;
            for attr in field.attribute_info {
                match attr {
                    Attribute::ConstantValue { const_value_index } => {
                        const_index = Some(const_value_index)
                    }
                    _ => continue,
                }
            }

            // todo: don't unwrap here
            let const_index = const_index.unwrap();

            let const_val = self
                .class_file
                .const_pool()
                .get(const_index as usize - 1)
                .unwrap();

            match const_val {
                PoolKind::Integer(i) => write!(self.buf, "{}", i),
                PoolKind::Long(i) => write!(self.buf, "{}", i),
                PoolKind::Float { bytes } => write!(self.buf, "{}", float_to_f32(*bytes)),
                PoolKind::Double {
                    high_bytes,
                    low_bytes,
                } => write!(self.buf, "{}", double_to_f64(*high_bytes, *low_bytes)),
                PoolKind::String(i) => write!(self.buf, "{}", i),
                _ => todo!(),
            }?;
        } else {
            AstVisitor::visit(
                self.fields.remove(&name).unwrap(),
                &mut self.indent,
                &mut self.buf,
            )?;
        }
        writeln!(self.buf, ";")?;

        Ok(())
    }

    fn visit_method_body(&mut self, method: &MethodInfo) -> JResult<()> {
        let tokens = method.code().unwrap().lex();
        let mut local_variables = HashMap::new();
        // when the method is not static, the first argument is an implicit `this`
        let arg_offset = if method.access_flags.is_static() {
            0
        } else {
            local_variables.insert(
                0,
                StackEntry::Ident(
                    "this".to_owned(),
                    Type::ClassName(self.class_file.class_name()?.to_owned()),
                ),
            );
            1
        };
        for (idx, arg) in method.args.iter().enumerate() {
            local_variables.insert(
                idx + arg_offset,
                StackEntry::Ident(format!("arg{}", idx + arg_offset), arg.clone()),
            );
        }

        let ast = Codegen {
            class: &mut self.class_file,
            stack: Vec::new(),
            local_variables,
            tokens,
            ast: Vec::new(),
            current_pos: 0,
            fields: &mut HashMap::new(),
            inside_init: false,
        }
        .codegen()?;

        for line in ast {
            self.indent.write(&mut self.buf)?;
            AstVisitor::visit(line, &mut self.indent, &mut self.buf)?;
            self.buf.write_all(b"\n")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassFile {
    pub(crate) version: (MajorVersion, u16),
    pub(crate) const_pool: Vec<PoolKind>,
    pub(crate) access_flags: ClassAccessFlags,
    pub(crate) this_class: u16,
    pub(crate) super_class: u16,
    pub(crate) interfaces: Vec<u16>,
    pub(crate) fields: Vec<FieldInfo>,
    pub(crate) methods: Vec<MethodInfo>,
    pub(crate) attributes: Vec<Attribute>,
}

/// Methods for creating `ClassFiles`s
impl ClassFile {
    pub fn from_bufreader<R: Read + BufRead>(reader: R) -> JResult<ClassFile> {
        ClassFileBuilder {
            reader,
            const_pool: Vec::new(),
        }
        .parse()
    }

    pub fn from_path<P: AsRef<std::path::Path>>(p: P) -> JResult<ClassFile> {
        ClassFile::from_bufreader(BufReader::new(File::open(p)?))
    }
}

/// Methods for accessing data contained within the `ClassFile`
impl ClassFile {
    #[must_use]
    pub const fn version(&self) -> (MajorVersion, u16) {
        self.version
    }

    #[must_use]
    pub const fn const_pool(&self) -> &Vec<PoolKind> {
        &self.const_pool
    }

    #[must_use]
    pub const fn access_flags(&self) -> ClassAccessFlags {
        self.access_flags
    }

    pub fn class_name(&self) -> JResult<&str> {
        match self.const_pool[usize::from(self.this_class - 1)] {
            PoolKind::Class(name_index) => match self.const_pool[usize::from(name_index - 1)] {
                PoolKind::Utf8(ref s) => Ok(s),
                _ => Err(ParseError::IndexError(line!())),
            },
            _ => Err(ParseError::IndexError(line!())),
        }
    }

    pub fn super_class_name(&self) -> JResult<&str> {
        match self.const_pool[usize::from(self.super_class - 1)] {
            PoolKind::Class(name_index) => match self.const_pool[usize::from(name_index - 1)] {
                PoolKind::Utf8(ref s) => Ok(s),
                _ => Err(ParseError::IndexError(line!())),
            },
            _ => Err(ParseError::IndexError(line!())),
        }
    }

    pub fn interfaces(&self) -> JResult<Vec<&String>> {
        let mut interfaces = Vec::new();
        for interface_idx in &self.interfaces {
            interfaces.push(match self.const_pool[usize::from(interface_idx - 1)] {
                PoolKind::Utf8(ref s) => s,
                _ => unimplemented!(),
            });
        }
        Ok(interfaces)
    }

    #[must_use]
    pub const fn fields(&self) -> &Vec<FieldInfo> {
        &self.fields
    }

    pub fn field_names(&self) -> JResult<Vec<&str>> {
        let mut fields = Vec::new();
        for field in &self.fields {
            fields.push(match self.const_pool[usize::from(field.name_index - 1)] {
                PoolKind::Utf8(ref s) => s.as_str(),
                _ => unimplemented!(),
            });
        }
        Ok(fields)
    }

    pub fn field_types(&self) -> JResult<Vec<&str>> {
        let mut fields = Vec::new();
        for field in &self.fields {
            fields.push(
                match self.const_pool[usize::from(field.descriptor_index - 1)] {
                    PoolKind::Utf8(ref s) => s.as_str(),
                    _ => unimplemented!(),
                },
            );
        }
        Ok(fields)
    }

    pub fn field_names_and_types(&self) -> JResult<Vec<(&str, &str)>> {
        Ok(self
            .field_names()?
            .into_iter()
            .zip(self.field_types()?)
            .collect())
    }

    #[must_use]
    pub const fn methods(&self) -> &Vec<MethodInfo> {
        &self.methods
    }

    #[must_use]
    pub fn method_names(&self) -> Vec<&String> {
        self.methods.iter().map(|m| &m.name).collect()
    }

    pub fn method_by_name(&self, name: &str) -> JResult<&MethodInfo> {
        for method in &self.methods {
            if method.name == name.as_ref() {
                return Ok(method);
            }
        }
        Err(ParseError::MethodNotFound)
    }

    #[must_use]
    pub fn methods_name_hash(&self) -> HashMap<&str, &MethodInfo> {
        let names = self.method_names();
        let mut hash = HashMap::new();
        for (ii, name) in names.iter().enumerate() {
            hash.insert(name.as_str(), &self.methods[ii]);
        }
        hash
    }

    #[must_use]
    pub const fn attributes(&self) -> &Vec<Attribute> {
        &self.attributes
    }

    #[must_use]
    pub fn source_file(&self) -> Option<&str> {
        for attr in &self.attributes {
            match attr {
                Attribute::SourceFile(idx) => match self.const_pool[usize::from(idx - 1)] {
                    PoolKind::Utf8(ref s) => return Some(s.as_str()),
                    _ => unimplemented!(),
                },
                _ => continue,
            }
        }
        None
    }

    pub fn class_name_from_index(&self, index: u16) -> JResult<String> {
        if let PoolKind::Class(i) = &self.const_pool[usize::from(index - 1)] {
            Ok(self.utf_from_index(*i)?)
        } else {
            Err(ParseError::IndexError(line!()))
        }
    }

    // todo: can return &'self str?
    pub fn utf_from_index(&self, index: u16) -> JResult<String> {
        if let PoolKind::Utf8(s) = &self.const_pool[usize::from(index - 1)] {
            Ok(s.clone())
        } else {
            Err(ParseError::IndexError(line!()))
        }
    }

    pub fn bootstrap_methods(&self) -> Option<&[BootstrapMethod]> {
        for attr in &self.attributes {
            match attr {
                Attribute::BootstrapMethods(b) => return Some(b),
                _ => continue,
            }
        }

        None
    }

    pub fn read_methodref_from_index(
        &self,
        index: u16,
    ) -> JResult<(String, String, MethodDescriptor)> {
        match &self.const_pool[usize::from(index - 1)] {
            PoolKind::MethodRef {
                name_and_type_index,
                class_index,
            } => {
                let (name, sig): (String, MethodDescriptor) = match &self.const_pool
                    [usize::from(name_and_type_index - 1)]
                {
                    PoolKind::NameAndType {
                        name_index,
                        descriptor_index,
                    } => {
                        let name = self.utf_from_index(*name_index)?;
                        let ty = MethodDescriptor::new(&self.utf_from_index(*descriptor_index)?);
                        (name, ty)
                    }
                    _ => return Err(ParseError::IndexError(line!())),
                };
                let class = self
                    .class_name_from_index(*class_index)?
                    .split('/')
                    .last()
                    .unwrap()
                    .to_owned();
                Ok((class, name, sig))
            }
            _ => Err(ParseError::IndexError(line!())),
        }
    }

    pub fn read_invoke_dynamic_from_index(
        &self,
        index: u16,
    ) -> JResult<(String, String, MethodDescriptor)> {
        match &self.const_pool[usize::from(index - 1)] {
            PoolKind::InvokeDynamic {
                name_and_type_index,
                boostrap_method_attr_index,
            } => {
                let (name, sig): (String, MethodDescriptor) = match &self.const_pool
                    [usize::from(name_and_type_index - 1)]
                {
                    PoolKind::NameAndType {
                        name_index,
                        descriptor_index,
                    } => {
                        let name = self.utf_from_index(*name_index)?;
                        let ty = MethodDescriptor::new(&self.utf_from_index(*descriptor_index)?);
                        (name, ty)
                    }
                    _ => return Err(ParseError::IndexError(line!())),
                };
                let bootstrap_method = self
                    .bootstrap_methods()
                    .unwrap()
                    .get(*boostrap_method_attr_index as usize)
                    .unwrap();

                let class_index =
                    match &self.const_pool[bootstrap_method.bootstrap_method_ref as usize] {
                        PoolKind::MethodRef { class_index, .. } => class_index,
                        _ => todo!("expected `PoolKind::MethodRef`"),
                    };

                let class = self
                    .class_name_from_index(*class_index)?
                    .split('/')
                    .last()
                    .unwrap()
                    .to_owned();
                Ok((class, name, sig))
            }
            _ => Err(ParseError::IndexError(line!())),
        }
    }

    pub fn read_fieldref_from_index(&self, index: u16) -> JResult<(String, String, Type)> {
        match &self.const_pool[usize::from(index - 1)] {
            PoolKind::FieldRef {
                name_and_type_index,
                class_index,
            } => {
                let (name, sig): (String, Type) = match &self.const_pool
                    [usize::from(name_and_type_index - 1)]
                {
                    PoolKind::NameAndType {
                        name_index,
                        descriptor_index,
                    } => {
                        let name = self.utf_from_index(*name_index)?;
                        let ty = FieldDescriptor::new(&self.utf_from_index(*descriptor_index)?).ty;
                        (name, ty)
                    }
                    _ => return Err(ParseError::IndexError(line!())),
                };
                let class = self
                    .class_name_from_index(*class_index)?
                    .split('/')
                    .last()
                    .unwrap()
                    .to_owned();
                Ok((class, name, sig))
            }
            _ => Err(ParseError::IndexError(line!())),
        }
    }
}

/// Methods for codegen and printing
impl ClassFile {
    pub fn print<W: Write>(self, buf: W) -> JResult<()> {
        let mut visitor = ClassFileVisitor {
            class_file: self,
            buf,
            indent: Indent::new(),
            fields: HashMap::new(),
        };

        visitor.visit_class()
    }

    pub fn class_signature(&self) -> JResult<String> {
        let mut s = Vec::new();
        if self.access_flags.is_public {
            s.push("public");
        }
        if self.access_flags.is_final {
            s.push("final");
        }
        if self.access_flags.is_abstract {
            s.push("abstract");
        }
        if self.access_flags.is_synthetic {
            s.push("synthetic");
        }
        if self.access_flags.is_annotation {
            s.push("annotation");
        }
        if self.access_flags.is_interface {
            s.push("interface");
        }
        if self.access_flags.is_enum {
            s.push("enum");
        } else {
            s.push("class");
        }
        s.push(self.class_name()?);
        if !self.access_flags.is_super {
            s.push("extends");
            s.push(self.super_class_name()?);
        }
        s.push("{\n");
        Ok(s.join(" "))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

    #[must_use]
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

impl std::fmt::Display for ClassAccessFlags {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}-{}-{}-{}-{}-{}-{}-{}",
            if self.is_public { "PUBLIC" } else { "" },
            if self.is_final { "FINAL" } else { "" },
            if self.is_super { "SUPER" } else { "" },
            if self.is_interface { "INTERFACE" } else { "" },
            if self.is_abstract { "ABSTRACT" } else { "" },
            if self.is_synthetic { "SYNTHETIC" } else { "" },
            if self.is_annotation { "ANNOTATION" } else { "" },
            if self.is_enum { "ENUM" } else { "" },
        )
    }
}
