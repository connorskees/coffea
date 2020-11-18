use std::{fmt, string::ToString};

use crate::common::{BinaryOp, Type, UnaryOp};
use crate::StackEntry;

/// A higher level representation of `StackEntry`
#[derive(Debug, Clone, PartialEq)]
pub enum AST {
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
    /// takes {ty: Type, num_of_els: usize, els: Vec<AST>}
    Array {
        ty: Type,
        num_of_els: usize,
        els: Vec<AST>,
    },
    /// array index
    /// takes { arr: Box<AST>, index: Box<AST>, arr_type: Type }
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
    UnaryOp(UnaryOp),
    If {
        cond: Box<AST>,
        then: Vec<AST>,
    },
    Goto(usize),
    NOP,
    StackEntry(StackEntry),
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
                num_of_els,
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
            StackEntry::UnaryOp(op) => AST::UnaryOp(*op),
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
            StackEntry::Unitialized => panic!("attempted to convert unitialized to AST"),
        }
    }
}

impl Into<Box<AST>> for StackEntry {
    fn into(self) -> Box<AST> {
        Box::new(self.into())
    }
}

impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AST::Null => write!(f, "null"),
            AST::Int(i) => write!(f, "{}", i),
            AST::Float(fl) => write!(f, "{}", fl),
            AST::Double(d) => write!(f, "{}", d),
            AST::Long(l) => write!(f, "{}", l),
            AST::String(s) => write!(f, "\"{}\"", s),
            AST::Object(o) => write!(f, "{}", o),
            AST::Array { els, .. } => write!(
                f,
                "{{ {} }}",
                els.iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            AST::ArrayIndex { arr, index, .. } => write!(f, "{}[{}]", arr, index),
            AST::Cast(ty, val) => write!(f, "({}) {}", ty, val),
            AST::Ident(s, _ty) => write!(f, "{}", s),
            AST::New(val) => write!(f, "new {}", val),
            AST::MethodCall(obj, meth) => write!(f, "{}.{}", obj, meth),
            AST::FunctionCall { name, args, .. } => write!(
                f,
                "{}({})",
                name,
                args.iter()
                    .rev()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            AST::FieldAccess {
                obj, field_name, ..
            } => write!(f, "{}.{}", obj, field_name),
            AST::FieldAssignment {
                obj,
                field_name,
                val,
                ..
            } => write!(f, "{}.{} = {};", obj, field_name, val),
            AST::Assignment(ty, name, val) => writeln!(f, "{} {} = {};", ty, name, val),
            AST::ReAssignment { var, val } => writeln!(f, "{} = {};", var, val),
            AST::UnaryOp(op) => write!(f, "{}", op),
            AST::BinaryOp(a, op, b) => write!(f, "({} {} {})", a, op, b),
            AST::If { cond, then } => writeln!(
                f,
                "if ({}) {{\n{}}}",
                cond,
                then.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<String>>()
                    .join("")
            ),
            AST::Goto(_) => panic!("attempted to render goto"),
            AST::NOP => write!(f, ""),
            AST::StackEntry(s) => write!(f, "{}", s),
            AST::Return(val) => match val {
                Some(v) => write!(f, "return {};", v),
                None => write!(f, "return;"),
            },
        }
    }
}
