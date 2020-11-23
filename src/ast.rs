use std::io::{self, Write};

use crate::common::{BinaryOp, Indent, Type, UnaryOp};
use crate::StackEntry;

/// A higher level representation of `StackEntry`
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AST {
    Null,
    /// integer literal
    Int(i32),
    /// float literal
    Float(f32),
    /// double literal
    Double(f64),
    /// long literal
    Long(i64),
    /// string literal
    String(String),
    /// object
    Object(String),
    /// array literal
    Array {
        ty: Type,
        num_of_els: Box<AST>,
        els: Vec<AST>,
    },
    /// array index
    ArrayIndex {
        arr: Box<AST>,
        index: Box<AST>,
        arr_type: Type,
    },
    Cast(Type, Box<AST>),
    Ident(String, Type),
    New(String),
    MethodCall(Box<AST>, Box<AST>),
    FunctionCall {
        name: String,
        args: Vec<AST>,
        return_type: Type,
    },
    FieldAccess {
        obj: Box<AST>,
        field_name: String,
        ty: Type,
    },
    FieldAssignment {
        obj: Box<AST>,
        field_name: String,
        ty: Type,
        val: Box<AST>,
    },
    Assignment(Type, String, Box<AST>),
    ReAssignment {
        var: Box<AST>,
        val: Box<AST>,
    },
    BinaryOp(Box<AST>, BinaryOp, Box<AST>),
    UnaryOp(UnaryOp, Box<AST>),
    If {
        cond: Box<AST>,
        then: Vec<AST>,
    },
    Return(Option<Box<AST>>),
}

impl From<StackEntry> for AST {
    fn from(entry: StackEntry) -> Self {
        match entry {
            StackEntry::Null => AST::Null,
            StackEntry::Int(i) => AST::Int(i),
            StackEntry::Float(f) => AST::Float(f),
            StackEntry::Double(d) => AST::Double(d),
            StackEntry::Long(l) => AST::Long(l),
            StackEntry::Array(ty, num_of_els, els) => AST::Array {
                ty,
                num_of_els: Box::new(AST::from(*num_of_els)),
                els: els.into_iter().map(|el| el.into()).collect(),
            },
            StackEntry::New(s) => AST::New(s),
            StackEntry::Index(arr, index, arr_type) => AST::ArrayIndex {
                arr: Box::new(AST::from(*arr)),
                index: Box::new(AST::from(*index)),
                arr_type,
            },
            StackEntry::Class(s) => AST::Object(s),
            StackEntry::Cast(ty, val) => AST::Cast(ty, Box::new(AST::from(*val))),
            StackEntry::UnaryOp(op, val) => AST::UnaryOp(op, Box::new(AST::from(*val))),
            StackEntry::BinaryOp(left, op, right) => {
                AST::BinaryOp(Box::new(AST::from(*left)), op, Box::new(AST::from(*right)))
            }
            StackEntry::Ident(name, ty) => AST::Ident(name, ty),
            StackEntry::Function(name, args, return_type) => AST::FunctionCall {
                name,
                args: args.into_iter().map(|arg| arg.into()).collect(),
                return_type,
            },
            StackEntry::Field(obj, field_name, ty) => AST::FieldAccess {
                obj: Box::new(AST::from(*obj)),
                field_name,
                ty,
            },
            StackEntry::String(s) => AST::String(s),
        }
    }
}

impl Into<Box<AST>> for StackEntry {
    fn into(self) -> Box<AST> {
        Box::new(self.into())
    }
}

#[derive(Debug)]
pub struct AstVisitor;

impl AstVisitor {
    pub fn new() -> Self {
        Self
    }

    // todo: pretty print e.g. `Long.MAX_VALUE`?
    pub(crate) fn visit(ast: AST, indent: &mut Indent, f: &mut dyn Write) -> io::Result<()> {
        match ast {
            AST::Null => write!(f, "null")?,
            AST::Int(i) => write!(f, "{}", i)?,
            AST::Float(fl) => write!(f, "{}", fl)?,
            AST::Double(d) => write!(f, "{}", d)?,
            AST::Long(l) => write!(f, "{}", l)?,
            AST::String(s) => write!(f, "\"{}\"", s)?,
            AST::Object(o) => write!(f, "{}", o)?,
            AST::Array {
                mut els,
                num_of_els,
                ty,
            } => {
                if els.is_empty() {
                    write!(f, "new {}[", ty)?;
                    AstVisitor::visit(*num_of_els, indent, f)?;
                    write!(f, "]")?;
                    return Ok(());
                }
                write!(f, "{{ ")?;

                let last = els.pop();

                for el in els {
                    AstVisitor::visit(el, indent, f)?;
                    write!(f, ", ")?;
                }

                if let Some(last) = last {
                    AstVisitor::visit(last, indent, f)?;
                }

                write!(f, " }}")?;
            }
            AST::ArrayIndex { arr, index, .. } => {
                AstVisitor::visit(*arr, indent, f)?;
                write!(f, "[")?;
                AstVisitor::visit(*index, indent, f)?;
                write!(f, "]")?;
            }
            AST::Cast(ty, val) => {
                write!(f, "({}) ", ty)?;
                AstVisitor::visit(*val, indent, f)?;
            }
            AST::Ident(s, _ty) => write!(f, "{}", s)?,
            AST::New(val) => write!(f, "new {}", val)?,
            AST::MethodCall(obj, meth) => {
                AstVisitor::visit(*obj, indent, f)?;
                write!(f, ".")?;
                AstVisitor::visit(*meth, indent, f)?;
            }
            AST::FunctionCall { name, mut args, .. } => {
                args.reverse();
                write!(f, "{}(", name)?;

                let last = args.pop();

                for el in args {
                    AstVisitor::visit(el, indent, f)?;
                    write!(f, ", ")?;
                }

                if let Some(last) = last {
                    AstVisitor::visit(last, indent, f)?;
                }

                write!(f, ")")?;
            }
            AST::FieldAccess {
                obj, field_name, ..
            } => {
                AstVisitor::visit(*obj, indent, f)?;
                write!(f, ".{}", field_name)?
            }
            AST::FieldAssignment {
                obj,
                field_name,
                val,
                ..
            } => {
                AstVisitor::visit(*obj, indent, f)?;
                write!(f, ".{} = ", field_name)?;
                AstVisitor::visit(*val, indent, f)?;
                write!(f, ";")?;
            }
            AST::Assignment(ty, name, val) => {
                write!(f, "{} {} = ", ty, name)?;
                AstVisitor::visit(*val, indent, f)?;
                write!(f, ";")?;
            }
            AST::ReAssignment { var, val } => {
                AstVisitor::visit(*var, indent, f)?;
                write!(f, " = ")?;
                AstVisitor::visit(*val, indent, f)?;
                write!(f, ";")?;
            }
            AST::UnaryOp(op, val) => {
                if op.is_prefix() {
                    write!(f, "{}", op)?;
                    AstVisitor::visit(*val, indent, f)?;
                } else {
                    AstVisitor::visit(*val, indent, f)?;
                    write!(f, "{}", op)?;
                }
            }
            AST::BinaryOp(a, op, b) => {
                write!(f, "(")?;
                AstVisitor::visit(*a, indent, f)?;
                write!(f, " {} ", op)?;
                AstVisitor::visit(*b, indent, f)?;
                write!(f, ")")?;
            }
            AST::If { cond, then } => {
                write!(f, "if (")?;
                AstVisitor::visit(*cond, indent, f)?;
                write!(f, ") {{\n")?;
                indent.increase();

                for line in then {
                    indent.write(f)?;
                    AstVisitor::visit(line, indent, f)?;
                    writeln!(f)?;
                }

                indent.decrease();

                indent.write(f)?;

                write!(f, "}}")?;
            }
            AST::Return(val) => match val {
                Some(v) => {
                    write!(f, "return ")?;
                    AstVisitor::visit(*v, indent, f)?;
                    write!(f, ";")?;
                }
                None => write!(f, "return;")?,
            },
        }

        Ok(())
    }
}
