use crate::attributes::Attribute;
use crate::code::Code;
use crate::common::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodInfo {
    pub access_flags: MethodAccessFlags,
    pub name: String,
    pub return_type: String,
    pub attributes: Vec<Attribute>,
}

#[derive(Debug, Clone)]
pub struct MethodSignature {
    pub args: Vec<Type>,
    pub return_type: Type,
}

impl MethodSignature {
    pub fn from_descriptor<S: AsRef<str>>(s: S) -> MethodSignature {
        let mut chars = s.as_ref().chars();
        let mut args: Vec<Type> = Vec::new();

        if let Some(c) = chars.next() {
            if c != '(' {
                unimplemented!("invalid starting character in return type")
            }
        }

        while let Some(c) = MethodSignature::eat_type(&mut chars) {
            args.push(c);
        }

        let ret = match MethodSignature::eat_type(&mut chars) {
            Some(t) => t,
            None => unimplemented!("no return type given"),
        };
        MethodSignature {
            return_type: ret,
            args,
        }
    }

    fn eat_type<'a>(cc: &mut std::str::Chars<'a>) -> Option<Type> {
        if let Some(c) = cc.next() {
            Some(match c {
                ')' => return None,
                'B' => Type::Byte,
                'C' => Type::Char,
                'D' => Type::Double,
                'F' => Type::Float,
                'I' => Type::Int,
                'J' => Type::Long,
                'L' => {
                    let mut name = String::new();
                    while let Some(c) = cc.next() {
                        if c == ';' {
                            break;
                        }
                        name.push(c);
                    }
                    Type::ClassName(name)
                }
                'S' => Type::Short,
                'Z' => Type::Boolean,
                'V' => Type::Void,
                '[' => {
                    let t = match MethodSignature::eat_type(cc) {
                        Some(t) => t.clone(),
                        None => unimplemented!(),
                    };
                    Type::Reference(Box::new(t))
                }
                _ => unimplemented!("unknown character"),
            })
        } else {
            None
        }
    }
}

impl MethodInfo {
    pub fn signature(&self) -> String {
        let mut string = String::with_capacity(100);
        let flags = self.access_flags;
        let mut attrs: Vec<&str> = Vec::new();
        if flags.is_public {
            attrs.push("public");
        }
        if flags.is_static {
            attrs.push("static");
        }
        if flags.is_final {
            attrs.push("final");
        }
        let signature = MethodSignature::from_descriptor(&self.return_type);
        let return_type = signature.return_type.as_string();
        let args = signature
            .args
            .iter()
            .map(|a| a.clone().as_string())
            .collect::<Vec<String>>()
            .join(", ");
        attrs.push(&return_type);
        let s: &str = &format!("{}({}) {{\n", &self.name, args);
        attrs.push(s);
        string.push_str(&attrs.join(" "));
        string
    }

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

    pub fn is_public(&self) -> bool {
        self.is_public
    }
    pub fn is_private(&self) -> bool {
        self.is_private
    }
    pub fn is_protected(&self) -> bool {
        self.is_protected
    }
    pub fn is_static(&self) -> bool {
        self.is_static
    }
    pub fn is_final(&self) -> bool {
        self.is_final
    }
    pub fn is_synchronized(&self) -> bool {
        self.is_synchronized
    }
    pub fn is_bridge(&self) -> bool {
        self.is_bridge
    }
    pub fn is_var_args(&self) -> bool {
        self.is_var_args
    }
    pub fn is_native(&self) -> bool {
        self.is_native
    }
    pub fn is_abstract(&self) -> bool {
        self.is_abstract
    }
    pub fn is_strict(&self) -> bool {
        self.is_strict
    }
    pub fn is_synthetic(&self) -> bool {
        self.is_synthetic
    }
}
