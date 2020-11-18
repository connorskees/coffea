use crate::{
    attributes::Attribute,
    code::Code,
    common::{parse_single_type, Type},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodInfo {
    pub access_flags: MethodAccessFlags,
    pub name: String,
    pub args: Box<[Type]>,
    pub return_type: Type,
    pub attributes: Box<[Attribute]>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct MethodDescriptor {
    pub args: Box<[Type]>,
    pub return_type: Type,
}

impl MethodDescriptor {
    /// Parse method descriptor from string
    pub fn new(s: &str) -> MethodDescriptor {
        let mut chars = s.chars();
        let mut args: Vec<Type> = Vec::new();

        if chars.next() != Some('(') {
            todo!("invalid starting character in return type")
        }

        while let Some(c) = parse_single_type(&mut chars) {
            args.push(c);
        }

        let ret = match parse_single_type(&mut chars) {
            Some(t) => t,
            None => todo!("no return type given"),
        };
        MethodDescriptor {
            return_type: ret,
            args: args.into_boxed_slice(),
        }
    }
}

impl MethodInfo {
    #[must_use]
    // todo: the rest of access_flags
    pub fn signature(&self) -> String {
        // todo: initialize with smarter capacity
        let mut signature = String::new();

        // when the method is not static, the first argument is an implicit `this`
        let mut arg_offset = 1_usize;
        if self.access_flags.is_public {
            signature.push_str("public ");
        }
        if self.access_flags.is_static {
            signature.push_str("static ");
            arg_offset = 0;
        }
        if self.access_flags.is_final {
            signature.push_str("final ");
        }

        signature.push_str(&self.return_type.as_str());
        signature.push(' ');
        signature.push_str(&self.name);
        signature.push('(');

        let args = self
            .args
            .iter()
            .enumerate()
            .map(|(i, a)| format!("{} arg{}", a, i + arg_offset))
            .collect::<Vec<String>>()
            .join(", ");

        signature.push_str(&args);
        signature.push_str(") {\n");

        signature
    }

    #[must_use]
    pub fn code(&self) -> Option<&Code> {
        for attr in self.attributes.iter() {
            match attr {
                Attribute::Code(c) => return Some(c),
                _ => continue,
            }
        }
        None
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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
    pub const PUBLIC: u16 = 0x0001;
    pub const PRIVATE: u16 = 0x0002;
    pub const PROTECTED: u16 = 0x0004;
    pub const STATIC: u16 = 0x0008;
    pub const FINAL: u16 = 0x0010;
    pub const SYNCHRONIZED: u16 = 0x0020;
    pub const BRIDGE: u16 = 0x0040;
    pub const VARARGS: u16 = 0x0080;
    pub const NATIVE: u16 = 0x0100;
    pub const ABSTRACT: u16 = 0x0400;
    pub const STRICT: u16 = 0x0800;
    pub const SYNTHETIC: u16 = 0x1000;

    #[must_use]
    pub const fn from_u16(n: u16) -> MethodAccessFlags {
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

    #[must_use]
    pub const fn is_public(&self) -> bool {
        self.is_public
    }

    #[must_use]
    pub const fn is_private(&self) -> bool {
        self.is_private
    }

    #[must_use]
    pub const fn is_protected(&self) -> bool {
        self.is_protected
    }

    #[must_use]
    pub const fn is_static(&self) -> bool {
        self.is_static
    }

    #[must_use]
    pub const fn is_final(&self) -> bool {
        self.is_final
    }

    #[must_use]
    pub const fn is_synchronized(&self) -> bool {
        self.is_synchronized
    }

    #[must_use]
    pub const fn is_bridge(&self) -> bool {
        self.is_bridge
    }

    #[must_use]
    pub const fn is_var_args(&self) -> bool {
        self.is_var_args
    }

    #[must_use]
    pub const fn is_native(&self) -> bool {
        self.is_native
    }

    #[must_use]
    pub const fn is_abstract(&self) -> bool {
        self.is_abstract
    }

    #[must_use]
    pub const fn is_strict(&self) -> bool {
        self.is_strict
    }

    #[must_use]
    pub const fn is_synthetic(&self) -> bool {
        self.is_synthetic
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn method_info_pubic_static_no_args() {
        let method_info = MethodInfo {
            access_flags: MethodAccessFlags::from_u16(
                MethodAccessFlags::PUBLIC | MethodAccessFlags::STATIC,
            ),
            name: "testMethod".to_owned(),
            args: Vec::new().into_boxed_slice(),
            return_type: Type::Void,
            attributes: Vec::new().into_boxed_slice(),
        };

        assert_eq!(
            method_info.signature(),
            "public static void testMethod() {\n"
        );
    }

    #[test]
    fn method_info_many_args() {
        let method_info = MethodInfo {
            access_flags: MethodAccessFlags::from_u16(0),
            name: "testMethod".to_owned(),
            args: vec![
                Type::Boolean,
                Type::Char,
                Type::Short,
                Type::Long,
                Type::Reference(Box::new(Type::Reference(Box::new(Type::Int)))),
            ]
            .into_boxed_slice(),
            return_type: Type::ClassName("String".to_owned()),
            attributes: Vec::new().into_boxed_slice(),
        };
        assert_eq!(
            method_info.signature(),
            "String testMethod(boolean arg1, char arg2, short arg3, long arg4, int[][] arg5) {\n"
        );
    }

    #[test]
    fn method_info_many_args_static() {
        let method_info = MethodInfo {
            access_flags: MethodAccessFlags::from_u16(MethodAccessFlags::STATIC),
            name: "testMethod".to_owned(),
            args: vec![
                Type::Boolean,
                Type::Char,
                Type::Short,
                Type::Long,
                Type::Reference(Box::new(Type::Reference(Box::new(Type::Int)))),
            ]
            .into_boxed_slice(),
            return_type: Type::ClassName("String".to_owned()),
            attributes: Vec::new().into_boxed_slice(),
        };
        assert_eq!(
            method_info.signature(),
            "static String testMethod(boolean arg0, char arg1, short arg2, long arg3, int[][] arg4) {\n"
        );
    }

    #[test]
    fn parse_method_descriptor_main() {
        assert_eq!(
            MethodDescriptor::new("([Ljava/lang/String;)V"),
            MethodDescriptor {
                args: vec![Type::Reference(Box::new(Type::string()))].into_boxed_slice(),
                return_type: Type::Void,
            }
        )
    }

    #[test]
    fn parse_method_descriptor_many_params() {
        assert_eq!(
            MethodDescriptor::new("(IDLjava/lang/String;)Ljava/lang/String;"),
            MethodDescriptor {
                args: vec![Type::Int, Type::Double, Type::string()].into_boxed_slice(),
                return_type: Type::string(),
            }
        )
    }

    #[test]
    fn parse_method_descriptor_custom_class_as_param() {
        assert_eq!(
            MethodDescriptor::new("(LTestClass;D)Ljava/lang/String;"),
            MethodDescriptor {
                args: vec![Type::ClassName("TestClass".to_owned()), Type::Double,]
                    .into_boxed_slice(),
                return_type: Type::string(),
            }
        )
    }

    #[test]
    fn parse_method_descriptor_nested_list() {
        assert_eq!(
            MethodDescriptor::new("([[ID[Ljava/lang/String;)V"),
            MethodDescriptor {
                args: vec![
                    Type::Reference(Box::new(Type::Reference(Box::new(Type::Int)))),
                    Type::Double,
                    Type::Reference(Box::new(Type::string()))
                ]
                .into_boxed_slice(),
                return_type: Type::Void,
            }
        )
    }
}
