#![deny(missing_debug_implementations)]
#![warn(clippy::pedantic)]
#![allow(dead_code, clippy::use_self, clippy::module_name_repetitions)]
// todo: heuristic for byte and friends being converted to int (e.g. indexing into array)
// todo: heuristic for generics
// todo: not necessary to have field descriptor (can just be type)
// todo: stringbuilder syntactic sugar
// todo: heuristic for variables initially declared as null
// todo: method calls as their own stack entry
// todo: i++ and i-- as expressions
// todo: i-- parses to i++
use std::{cmp::Ordering, collections::HashMap, convert::TryInto, fmt, string::ToString};

use crate::{
    ast::AST,
    code::{Instruction, Instructions},
    errors::{JResult, ParseError},
    invoke_dynamic::{ArgType, InvokeDynamicArgs},
};
pub use crate::{
    classfile::{ClassAccessFlags, ClassFile},
    common::{double_to_f64, float_to_f32, BinaryOp, Type, UnaryOp},
    fields::{FieldAccessFlags, FieldDescriptor, FieldInfo},
    pool::PoolKind,
    version::MajorVersion,
};

pub mod ast;
pub mod attributes;
mod builder;
mod classfile;
pub mod code;
mod common;
pub mod errors;
mod fields;
mod invoke_dynamic;
pub mod methods;
mod pool;
mod version;

#[derive(Debug, Clone, PartialEq)]
pub enum StackEntry {
    Null,
    Int(i32),
    Float(f32),
    Double(f64),
    Long(i64),
    /// type, length, elements
    Array(Type, usize, Vec<StackEntry>),
    /// name
    New(String),
    /// array, index, array type
    Index(Box<StackEntry>, Box<StackEntry>, Type),
    /// name
    Class(String),
    /// type_to, castee
    Cast(Type, Box<StackEntry>),
    UnaryOp(Box<UnaryOp>),
    /// lhs, op, rhs
    BinaryOp(Box<StackEntry>, BinaryOp, Box<StackEntry>),
    /// name, type
    Ident(String, Type),
    /// name, args, return type
    Function(String, Vec<StackEntry>, Type),
    /// class, field name, field type
    Field(Box<StackEntry>, String, Type),
    String(String),
    Unitialized,
}

impl StackEntry {
    fn ty(&self) -> Type {
        match self {
            StackEntry::Null => Type::ClassName("null".to_owned()),
            StackEntry::Int(_) => Type::Int,
            StackEntry::Float(_) => Type::Float,
            StackEntry::Double(_) => Type::Double,
            StackEntry::Long(_) => Type::Long,
            StackEntry::Array(ty, ..) => Type::Reference(Box::new(ty.clone())),
            StackEntry::New(s) | StackEntry::Class(s) => Type::ClassName(s.clone()),
            StackEntry::UnaryOp(op) => op.ty(),
            StackEntry::BinaryOp(left, _, _) => left.ty(),
            StackEntry::Index(_, _, ty)
            | StackEntry::Cast(ty, _)
            | StackEntry::Ident(_, ty)
            | StackEntry::Function(_, _, ty)
            | StackEntry::Field(_, _, ty) => ty.clone(),
            StackEntry::String(_) => Type::ClassName("String".to_owned()),
            StackEntry::Unitialized => panic!("attempted to get type of unitialized"),
        }
    }
}

impl fmt::Display for StackEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StackEntry::Null => write!(f, "null"),
            StackEntry::Int(a) => write!(f, "{}", a),
            StackEntry::Long(a) => write!(f, "{}l", a),
            StackEntry::Float(a) => write!(f, "{}f", a),
            StackEntry::Double(a) => write!(f, "{}d", a),
            StackEntry::Array(_, _, els) => write!(
                f,
                "{{ {} }}",
                els.iter()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            StackEntry::New(val) => write!(f, "new {}", val),
            StackEntry::Index(arr, idx, _ty) => write!(f, "{}[{}]", arr, idx),
            StackEntry::Class(name) => write!(f, "{}", name),
            StackEntry::Cast(ty, val) => write!(f, "({}) {}", ty, val),
            StackEntry::UnaryOp(op) => write!(f, "{}", op),
            StackEntry::BinaryOp(a, op, b) => write!(f, "({} {} {})", a, op, b),
            StackEntry::Ident(s, _ty) => write!(f, "{}", s),
            StackEntry::String(s) => write!(f, "\"{}\"", s),
            StackEntry::Function(name, args, _) => write!(
                f,
                "{}({})",
                name,
                args.iter()
                    .rev()
                    .map(|a| format!("{}", a))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            StackEntry::Field(_, name, _) => write!(f, "{}", name),
            StackEntry::Unitialized => panic!("attempted to render unitialized entry"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Codegen<'a> {
    class: &'a mut ClassFile,
    stack: Vec<StackEntry>,
    local_variables: HashMap<usize, StackEntry>,
    tokens: Instructions,
    ast: Vec<AST>,
    current_pos: i16,
    /// whether this is generating code for `<init>` or not
    inside_init: bool,
    fields: &'a mut HashMap<String, AST>,
}

impl Codegen<'_> {
    fn codegen(mut self) -> JResult<Vec<AST>> {
        while let Some(instruction) = self.tokens.next() {
            let val = self.read_instruction(instruction)?;
            match val {
                Some(a) => self.ast.push(a),
                None => continue,
            }
        }

        Ok(self.ast)
    }

    fn read_instruction(&mut self, instruction: Instruction) -> JResult<Option<AST>> {
        // dbg!(&instruction);
        let len = instruction.len();
        self.current_pos += len;
        match instruction {
            Instruction::BiPush(n) => self.stack.push(StackEntry::Int(i32::from(n))),
            Instruction::SIPush(n) => self.stack.push(StackEntry::Int(i32::from(n))),
            Instruction::Ldc(n) | Instruction::LdcW(n) => {
                let val = &self.class.const_pool[usize::from(n - 1)];
                match val {
                    PoolKind::String(idx) => self
                        .stack
                        .push(StackEntry::String(self.class.utf_from_index(*idx)?)),
                    PoolKind::Integer(i) => self.stack.push(StackEntry::Int(*i as i32)),
                    PoolKind::Float { bytes } => {
                        self.stack.push(StackEntry::Float(float_to_f32(*bytes)))
                    }
                    _ => unimplemented!(),
                }
            }
            Instruction::Ldc2W(n) => {
                let val = &self.class.const_pool[usize::from(n - 1)];

                match val {
                    PoolKind::Double {
                        high_bytes,
                        low_bytes,
                    } => {
                        self.stack
                            .push(StackEntry::Double(double_to_f64(*high_bytes, *low_bytes)));
                    }
                    PoolKind::Long(long) => self.stack.push(StackEntry::Long(*long)),
                    _ => unimplemented!("Ldc2w should only encounter doubles and longs"),
                }
            }
            Instruction::AConstNull => self.stack.push(StackEntry::Null),
            Instruction::IConstM1 => self.stack.push(StackEntry::Int(-1)),
            Instruction::IConst0 => self.stack.push(StackEntry::Int(0)),
            Instruction::IConst1 => self.stack.push(StackEntry::Int(1)),
            Instruction::IConst2 => self.stack.push(StackEntry::Int(2)),
            Instruction::IConst3 => self.stack.push(StackEntry::Int(3)),
            Instruction::IConst4 => self.stack.push(StackEntry::Int(4)),
            Instruction::IConst5 => self.stack.push(StackEntry::Int(5)),
            Instruction::FConst0 => self.stack.push(StackEntry::Float(0.0)),
            Instruction::FConst1 => self.stack.push(StackEntry::Float(1.0)),
            Instruction::FConst2 => self.stack.push(StackEntry::Float(2.0)),
            Instruction::DConst0 => self.stack.push(StackEntry::Double(0.0)),
            Instruction::DConst1 => self.stack.push(StackEntry::Double(1.0)),
            Instruction::LConst0 => self.stack.push(StackEntry::Long(0)),
            Instruction::LConst1 => self.stack.push(StackEntry::Long(1)),

            Instruction::ALoad(n)
            | Instruction::FLoad(n)
            | Instruction::DLoad(n)
            | Instruction::ILoad(n)
            | Instruction::LLoad(n) => self.load(n),
            Instruction::Aload0
            | Instruction::FLoad0
            | Instruction::DLoad0
            | Instruction::ILoad0
            | Instruction::LLoad0 => self.load(0),
            Instruction::Aload1
            | Instruction::FLoad1
            | Instruction::DLoad1
            | Instruction::ILoad1
            | Instruction::LLoad1 => self.load(1),
            Instruction::Aload2
            | Instruction::FLoad2
            | Instruction::DLoad2
            | Instruction::ILoad2
            | Instruction::LLoad2 => self.load(2),
            Instruction::ALoad3
            | Instruction::FLoad3
            | Instruction::DLoad3
            | Instruction::ILoad3
            | Instruction::LLoad3 => self.load(3),
            Instruction::AALoad
            | Instruction::BALoad
            | Instruction::CALoad
            | Instruction::DALoad
            | Instruction::FALoad
            | Instruction::IALoad
            | Instruction::LALoad
            | Instruction::SALoad => {
                let index = self.pop_stack()?;
                let array = self.pop_stack()?;
                let ty = array.ty();
                let ty = match ty {
                    Type::Reference(r) => *r,
                    _ => ty,
                };
                self.stack
                    .push(StackEntry::Index(Box::new(array), Box::new(index), ty))
            }

            Instruction::IStore(n)
            | Instruction::FStore(n)
            | Instruction::DStore(n)
            | Instruction::LStore(n)
            | Instruction::AStore(n) => return self.store(n),
            Instruction::IStore0
            | Instruction::FStore0
            | Instruction::DStore0
            | Instruction::LStore0
            | Instruction::AStore0 => return self.store(0),
            Instruction::IStore1
            | Instruction::FStore1
            | Instruction::DStore1
            | Instruction::LStore1
            | Instruction::AStore1 => return self.store(1),
            Instruction::IStore2
            | Instruction::FStore2
            | Instruction::DStore2
            | Instruction::LStore2
            | Instruction::AStore2 => return self.store(2),
            Instruction::IStore3
            | Instruction::FStore3
            | Instruction::DStore3
            | Instruction::LStore3
            | Instruction::AStore3 => return self.store(3),
            Instruction::AAStore
            | Instruction::BAStore
            | Instruction::CAStore
            | Instruction::DAStore
            | Instruction::FAStore
            | Instruction::IAStore
            | Instruction::LAStore
            | Instruction::SAStore => {
                let val = self.pop_stack()?;
                let index = self.pop_stack()?;
                let array = self.pop_stack()?;
                match array {
                    // this is used to fill values in array literal
                    StackEntry::Array(ty, count, mut els) => {
                        let index: usize = match index {
                            StackEntry::Int(i) => i.try_into()?,
                            _ => unimplemented!("non-int array index"),
                        };
                        els.push(val);
                        els.swap_remove(index);
                        self.stack.push(StackEntry::Array(ty, count, els));
                    }
                    StackEntry::Ident(s, ty) => {
                        return Ok(Some(AST::ReAssignment {
                            var: Box::new(AST::ArrayIndex {
                                arr: Box::new(AST::Ident(s, ty.clone())),
                                index: index.into(),
                                arr_type: ty,
                            }),
                            val: val.into(),
                        }))
                    }
                    _ => unimplemented!(),
                }
            }

            Instruction::IAdd | Instruction::FAdd | Instruction::DAdd | Instruction::LAdd => {
                self.binary_op(BinaryOp::Add)?
            }
            Instruction::ISub | Instruction::FSub | Instruction::DSub | Instruction::LSub => {
                self.binary_op(BinaryOp::Sub)?
            }
            Instruction::IMul | Instruction::FMul | Instruction::DMul | Instruction::LMul => {
                self.binary_op(BinaryOp::Mul)?
            }
            Instruction::IDiv | Instruction::FDiv | Instruction::DDiv | Instruction::LDiv => {
                self.binary_op(BinaryOp::Div)?
            }
            Instruction::IRem | Instruction::FRem | Instruction::DRem | Instruction::LRem => {
                self.binary_op(BinaryOp::Rem)?
            }
            Instruction::IShl | Instruction::LShl => self.binary_op(BinaryOp::Shl)?,
            Instruction::IShr | Instruction::LShr => self.binary_op(BinaryOp::Shr)?,
            Instruction::IUShr | Instruction::LUShr => self.binary_op(BinaryOp::UShr)?,
            Instruction::IXor | Instruction::LXor => self.binary_op(BinaryOp::Xor)?,
            Instruction::IAnd | Instruction::LAnd => self.binary_op(BinaryOp::And)?,
            Instruction::IOr | Instruction::LOr => self.binary_op(BinaryOp::Or)?,
            Instruction::Fcmpg | Instruction::Dcmpg => self.binary_op(BinaryOp::GreaterThan)?,
            Instruction::Fcmpl | Instruction::Dcmpl => self.binary_op(BinaryOp::LessThan)?,
            Instruction::INeg | Instruction::FNeg | Instruction::DNeg | Instruction::LNeg => {
                let val = self.pop_stack()?;
                self.stack
                    .push(StackEntry::UnaryOp(Box::new(UnaryOp::Neg(val))));
            }
            Instruction::InstanceOf(idx) => {
                let obj1 = self.pop_stack()?;
                let obj2 = self.class.class_name_from_index(idx)?;
                self.stack.push(StackEntry::BinaryOp(
                    Box::new(obj1),
                    BinaryOp::InstanceOf,
                    Box::new(StackEntry::Class(obj2)),
                ));
            }

            Instruction::Lcmp => {
                let val2 = match self.pop_stack()? {
                    StackEntry::Long(l) => l,
                    _ => unimplemented!("Lcmp non-long value"),
                };
                let val1 = match self.pop_stack()? {
                    StackEntry::Long(l) => l,
                    _ => unimplemented!("Lcmp non-long value"),
                };
                self.stack.push(match val1.cmp(&val2) {
                    Ordering::Equal => StackEntry::Int(0),
                    Ordering::Greater => StackEntry::Int(1),
                    Ordering::Less => StackEntry::Int(-1),
                });
            }

            Instruction::Return => {
                return Ok(Some(AST::Return(None)));
            }
            Instruction::AReturn
            | Instruction::Ireturn
            | Instruction::Freturn
            | Instruction::Dreturn
            | Instruction::Lreturn => {
                return Ok(Some(AST::Return(Some(self.pop_stack()?.into()))));
            }

            Instruction::InvokeDynamic(index, _, _) => {
                let (class, name, descriptor, unparsed_args) =
                    self.class.read_invoke_dynamic_from_index(index)?;

                if class == "StringConcatFactory" && name == "makeConcatWithConstants" {
                    let args = unparsed_args.to_vec();
                    self.string_concat_factory(args)?;
                    return Ok(None);
                }

                let mut args = Vec::new();

                for _ in 0..descriptor.args.len() {
                    args.push(self.pop_stack()?);
                }

                let f = StackEntry::Function(
                    format!("{}.{}", class, name),
                    args,
                    descriptor.return_type,
                );

                self.stack.push(f);
            }
            Instruction::InvokeInterface(_, _, _) => {
                unimplemented!("instruction `InvokeInterface` not yet implemented")
            }

            Instruction::InvokeSpecial(index) => {
                let (class, name, descriptor) = self.class.read_methodref_from_index(index)?;
                let mut args: Vec<StackEntry> = Vec::new();
                for _ in 0..descriptor.args.len() {
                    args.push(self.pop_stack()?);
                }
                let object = self.pop_stack()?;
                let f = StackEntry::Function(
                    match name.as_str() {
                        "<init>" | "<clinit>" => object.to_string(),
                        _ => format!("{}.{}", object, name),
                    },
                    args,
                    Type::ClassName(class),
                );
                self.stack.push(f);
            }

            Instruction::InvokeStatic(index) => {
                let (class, name, descriptor) = self.class.read_methodref_from_index(index)?;
                let mut args: Vec<StackEntry> = Vec::new();
                for _ in 0..descriptor.args.len() {
                    args.push(self.pop_stack()?);
                }
                let f = StackEntry::Function(
                    format!("{}.{}", class, name),
                    args.clone(),
                    descriptor.return_type.clone(),
                );
                if descriptor.return_type == Type::Void {
                    return Ok(Some(AST::MethodCall(
                        Box::new(AST::Object(class)),
                        Box::new(AST::FunctionCall {
                            name,
                            args: args.into_iter().map(|a| a.into()).collect(),
                            return_type: descriptor.return_type,
                        }),
                    )));
                }
                self.stack.push(f);
            }

            Instruction::InvokeVirtual(index) => {
                let (_, name, descriptor) = self.class.read_methodref_from_index(index)?;
                let mut args: Vec<StackEntry> = Vec::new();
                for _ in 0..descriptor.args.len() {
                    args.push(self.pop_stack()?);
                }
                let object = self.pop_stack()?;
                let f = StackEntry::Function(
                    format!("{}.{}", object, name),
                    args.clone(),
                    descriptor.return_type.clone(),
                );
                if descriptor.return_type == Type::Void {
                    return Ok(Some(AST::MethodCall(
                        Box::new(object.into()),
                        Box::new(AST::FunctionCall {
                            name,
                            args: args.into_iter().map(|a| a.into()).collect(),
                            return_type: descriptor.return_type,
                        }),
                    )));
                }
                self.stack.push(f);
            }

            Instruction::GetStatic(index) => {
                let (class, name, ty) = self.class.read_fieldref_from_index(index)?;
                self.stack.push(StackEntry::Field(
                    Box::new(StackEntry::Class(class)),
                    name,
                    ty,
                ));
            }
            Instruction::GetField(index) => {
                let (_, name, ty) = self.class.read_fieldref_from_index(index)?;
                let obj = self.pop_stack()?;
                match &obj {
                    StackEntry::Ident(_, _) => {
                        self.stack.push(StackEntry::Field(Box::new(obj), name, ty));
                    }
                    _ => unimplemented!("non-ident field access"),
                }
            }

            Instruction::PutStatic(index) => {
                let val = self.pop_stack()?;
                let (obj, field_name, ty) = self.class.read_fieldref_from_index(index)?;
                return Ok(Some(AST::FieldAssignment {
                    obj: Box::new(AST::Object(obj)),
                    field_name,
                    val: val.into(),
                    ty,
                }));
            }
            Instruction::PutField(index) => {
                let val = self.pop_stack()?;
                let obj = self.pop_stack()?;
                let (_, field_name, ty) = self.class.read_fieldref_from_index(index)?;

                if self.inside_init {
                    self.fields.insert(field_name.clone(), val.clone().into());
                }

                return Ok(Some(AST::FieldAssignment {
                    obj: obj.into(),
                    field_name,
                    val: val.into(),
                    ty,
                }));
            }

            Instruction::NewArray(ty) => {
                let ty = match ty {
                    4 => Type::Boolean, //int
                    5 => Type::Char,    //int
                    6 => Type::Float,   //float
                    7 => Type::Double,  //double
                    8 => Type::Byte,    //int
                    9 => Type::Short,   //int
                    10 => Type::Int,    //int
                    11 => Type::Long,   //long
                    _ => unimplemented!("unexpected NewArray type"),
                };
                let count: usize = match self.pop_stack()? {
                    StackEntry::Int(i) => i,
                    _ => unimplemented!("NewArray count is non-integer value"),
                }
                .try_into()?;
                let v = vec![StackEntry::Unitialized; count];
                self.stack.push(StackEntry::Array(ty, count, v))
            }
            Instruction::ANewArray(index) => {
                let ty = FieldDescriptor::new(&self.class.class_name_from_index(index)?).ty;
                let count: usize = match self.pop_stack()? {
                    StackEntry::Int(i) => i,
                    _ => unimplemented!("ANewArray count is non-integer value"),
                }
                .try_into()?;
                let v = vec![StackEntry::Unitialized; count];
                self.stack.push(StackEntry::Array(ty, count, v))
            }
            Instruction::MultiANewArray(_, _, _) => {
                unimplemented!("instruction `MultiANewArray` not yet implemented")
            }
            Instruction::ArrayLength => {
                let val = self.pop_stack()?;
                self.stack
                    .push(StackEntry::UnaryOp(Box::new(UnaryOp::ArrayLength(val))));
            }

            Instruction::Nop | Instruction::NoName => {}
            Instruction::Pop => return Ok(Some(self.stack.pop().unwrap().into())),
            Instruction::Pop2 => {
                let val1 = self.pop_stack()?;
                match val1.ty() {
                    Type::Long | Type::Double => {}
                    _ => return Ok(Some(self.stack.pop().unwrap().into())),
                }
                return Ok(Some(val1.into()));
            }

            Instruction::Iinc(idx, b) => {
                let val = self
                    .local_variables
                    .entry(usize::from(idx))
                    .or_insert(StackEntry::Int(-1));
                match val {
                    StackEntry::Int(u) => {
                        *val = StackEntry::Int(*u + i32::from(b));
                    }
                    StackEntry::Ident(..) => {
                        return Ok(Some(if b == 1 {
                            AST::UnaryOp(UnaryOp::PlusPlus(val.clone()))
                        } else {
                            AST::ReAssignment {
                                var: Box::new(val.clone().into()),
                                val: Box::new(AST::BinaryOp(
                                    Box::new(val.clone().into()),
                                    BinaryOp::Add,
                                    Box::new(AST::Int(i32::from(b))),
                                )),
                            }
                        }))
                    }
                    _ => unimplemented!("iinc unknown variable type"),
                }
            }

            Instruction::IfAcmpeq(_) => {
                unimplemented!("instruction `IfAcmpeq` not yet implemented")
            }
            Instruction::IfAcmpne(_) => {
                unimplemented!("instruction `IfAcmpne` not yet implemented")
            }
            Instruction::Ifeq(offset) => {
                let mut cond = self.pop_stack()?.into();
                let pos = self.current_pos + offset;
                let mut then = Vec::new();
                while let Some(tok) = self.tokens.next() {
                    let ast = self.read_instruction(tok)?;
                    match ast {
                        Some(v) => then.push(v),
                        None => continue,
                    }
                    if self.current_pos <= pos {
                        break;
                    }
                }

                if then.len() == 1 {
                    if let AST::If {
                        cond: this_cond,
                        then: this_then,
                    } = then[0].clone()
                    {
                        then = this_then;
                        cond = Box::new(AST::BinaryOp(cond, BinaryOp::LogicalAnd, this_cond));
                    }
                }
                return Ok(Some(AST::If { cond, then }));
            }
            Instruction::Ifge(_) => unimplemented!("instruction `Ifge` not yet implemented"),
            Instruction::Ifgt(_) => unimplemented!("instruction `Ifgt` not yet implemented"),
            Instruction::Ifle(_) => unimplemented!("instruction `Ifle` not yet implemented"),
            Instruction::Iflt(_) => unimplemented!("instruction `Iflt` not yet implemented"),
            Instruction::Ifne(offset) => {
                let raw_cond = self.pop_stack()?;
                let pos = self.current_pos + offset;
                let mut then = Vec::new();
                while let Some(tok) = self.tokens.next() {
                    let ast = self.read_instruction(tok)?;
                    match ast {
                        Some(v) => then.push(v),
                        None => continue,
                    }
                    if self.current_pos <= pos {
                        break;
                    }
                }

                let cond = if then.len() != 1 {
                    Box::new(AST::UnaryOp(UnaryOp::Negate(raw_cond)))
                } else {
                    match then[0].clone() {
                        AST::If {
                            cond: this_cond,
                            then: this_then,
                        } => {
                            then = this_then;
                            Box::new(AST::BinaryOp(
                                raw_cond.into(),
                                BinaryOp::LogicalOr,
                                this_cond,
                            ))
                        }
                        _ => Box::new(AST::UnaryOp(UnaryOp::Negate(raw_cond))),
                    }
                };
                return Ok(Some(AST::If { cond, then }));
            }
            Instruction::IfIcmpeq(offset) => {
                let val2 = self.pop_stack()?;
                let val1 = self.pop_stack()?;
                let mut cond =
                    Box::new(AST::BinaryOp(val1.into(), BinaryOp::NotEqual, val2.into()));
                let pos = self.current_pos + offset;
                let mut then = Vec::new();
                while let Some(tok) = self.tokens.next() {
                    let ast = self.read_instruction(tok)?;
                    match ast {
                        Some(v) => then.push(v),
                        None => continue,
                    }
                    if self.current_pos <= pos {
                        break;
                    }
                }

                if then.len() == 1 {
                    if let AST::If {
                        cond: this_cond,
                        then: this_then,
                    } = then[0].clone()
                    {
                        then = this_then;
                        cond = Box::new(AST::BinaryOp(cond, BinaryOp::LogicalOr, this_cond));
                    }
                }
                return Ok(Some(AST::If { cond, then }));
            }
            Instruction::IfIcmpne(offset) => {
                let val2 = self.pop_stack()?;
                let val1 = self.pop_stack()?;
                let cond = Box::new(AST::BinaryOp(val1.into(), BinaryOp::Equal, val2.into()));
                let pos = self.current_pos + offset;
                let mut then = Vec::new();
                while let Some(tok) = self.tokens.next() {
                    let ast = self.read_instruction(tok)?;
                    match ast {
                        Some(v) => then.push(v),
                        None => continue,
                    }
                    if self.current_pos <= pos {
                        break;
                    }
                }

                let cond = if then.len() != 1 {
                    cond
                } else {
                    match then[0].clone() {
                        AST::If {
                            cond: this_cond,
                            then: this_then,
                        } => {
                            then = this_then;
                            Box::new(AST::BinaryOp(cond, BinaryOp::LogicalAnd, this_cond))
                        }
                        _ => cond,
                    }
                };

                return Ok(Some(AST::If { cond, then }));
            }
            Instruction::IfIcmpge(_) => {
                unimplemented!("instruction `IfIcmpge` not yet implemented")
            }
            Instruction::IfIcmpgt(_) => {
                unimplemented!("instruction `IfIcmpgt` not yet implemented")
            }
            Instruction::IfIcmple(_) => {
                unimplemented!("instruction `IfIcmple` not yet implemented")
            }
            Instruction::IfIcmplt(_) => {
                unimplemented!("instruction `IfIcmplt` not yet implemented")
            }
            Instruction::Ifnonnull(_) => {
                unimplemented!("instruction `Ifnonnull` not yet implemented")
            }
            Instruction::Ifnull(_) => unimplemented!("instruction `Ifnull` not yet implemented"),

            Instruction::Goto(offset) => {
                self.tokens
                    .goto(&((offset + self.current_pos - len) as usize));
                // if self.tokens.next() == Some(instruction) {
                //     dbg!("test");
                // }
                // let offset: i16 = i16::from(branchbyte1) << 8 | i16::from(branchbyte2);
                // dbg!((offset + self.current_pos - len) as usize);
                // unimplemented!("goto is unimplemented")
            }
            Instruction::GotoW(_) => unimplemented!("instruction `GotoW` not yet implemented"),
            Instruction::Jsr(_) => unimplemented!("instruction `Jsr` not yet implemented"),
            Instruction::JsrW(_) => unimplemented!("instruction `JsrW` not yet implemented"),
            Instruction::Ret(_) => unimplemented!("instruction `Ret` not yet implemented"),

            Instruction::I2b => self.cast(Type::Byte)?,
            Instruction::I2c => self.cast(Type::Char)?,
            Instruction::I2d | Instruction::F2d | Instruction::L2d => self.cast(Type::Double)?,
            Instruction::I2l | Instruction::F2l | Instruction::D2l => self.cast(Type::Long)?,
            Instruction::I2s => self.cast(Type::Short)?,
            Instruction::F2i | Instruction::D2i | Instruction::L2i => self.cast(Type::Int)?,
            Instruction::I2f | Instruction::D2f | Instruction::L2f => self.cast(Type::Float)?,

            Instruction::New(idx) => {
                let obj = self.class.class_name_from_index(idx)?;
                self.stack.push(StackEntry::New(obj));
            }

            Instruction::Dup => {
                // todo: figure out initialization with `dup`
                let val = self.pop_stack()?;
                match val {
                    StackEntry::Array(..) | StackEntry::New(..) => self.stack.push(val),
                    _ => {
                        self.stack.push(val.clone());
                        self.stack.push(val);
                    }
                };
            }
            Instruction::DupX1 => {
                let val1 = self.pop_stack()?;
                let val2 = self.pop_stack()?;
                self.stack.push(val1.clone());
                self.stack.push(val2);
                self.stack.push(val1);
            }
            Instruction::DupX2 => {
                let val1 = self.pop_stack()?;
                let val2 = self.pop_stack()?;
                let val3 = self.pop_stack()?;
                self.stack.push(val1.clone());
                self.stack.push(val3);
                self.stack.push(val2);
                self.stack.push(val1);
            }
            Instruction::Dup2 => {
                let val1 = self.pop_stack()?;
                let val2 = self.pop_stack()?;
                self.stack.push(val2.clone());
                self.stack.push(val1.clone());
                self.stack.push(val2);
                self.stack.push(val1);
            }
            Instruction::Dup2X1 => {
                let val1 = self.pop_stack()?;
                let val2 = self.pop_stack()?;
                let val3 = self.pop_stack()?;
                self.stack.push(val2.clone());
                self.stack.push(val1.clone());
                self.stack.push(val3);
                self.stack.push(val2);
                self.stack.push(val1);
            }
            Instruction::Dup2X2 => {
                let val1 = self.pop_stack()?;
                let val2 = self.pop_stack()?;
                let val3 = self.pop_stack()?;
                let val4 = self.pop_stack()?;
                self.stack.push(val2.clone());
                self.stack.push(val1.clone());
                self.stack.push(val4);
                self.stack.push(val3);
                self.stack.push(val2);
                self.stack.push(val1);
            }

            Instruction::Swap => {
                let val1 = self.pop_stack()?;
                let val2 = self.pop_stack()?;
                self.stack.push(val1);
                self.stack.push(val2);
            }

            Instruction::Checkcast(_index) => {
                unimplemented!("instruction `Checkcast` not yet implemented")
            }

            Instruction::Athrow => unimplemented!("instruction `Athrow` not yet implemented"),

            // used only in debugging
            Instruction::Breakpoint | Instruction::Impdep1 | Instruction::Impdep2 => {}
            // low priority and complex
            Instruction::Lookupswitch
            | Instruction::MonitorEnter
            | Instruction::MonitorExit
            | Instruction::Wide3(_, _, _)
            | Instruction::Wide5(_, _, _, _, _)
            | Instruction::TableSwitch => unimplemented!("instruction not yet implemented"),
        };
        Ok(None)
    }

    /// reverse java/lang/invoke/StringConcatFactory.makeConcatWithConstants
    /// it is common enough to warrant special-cased desugaring
    fn string_concat_factory(&mut self, unparsed_args: Vec<u16>) -> JResult<()> {
        let mut args = Vec::new();
        for arg in unparsed_args {
            let s = self.class.string_from_index(arg)?;
            let mut parser = InvokeDynamicArgs::new(&s);

            while let Some(kind) = parser.next() {
                match kind {
                    ArgType::Pool(s) => {
                        args.push(StackEntry::String(s.to_owned()));
                    }
                    ArgType::Stack => args.push(self.pop_stack()?),
                }
            }
        }

        let mut entry = StackEntry::BinaryOp(
            Box::new(args.pop().unwrap()),
            BinaryOp::Add,
            Box::new(args.pop().unwrap()),
        );

        while let Some(arg) = args.pop() {
            entry = StackEntry::BinaryOp(Box::new(entry), BinaryOp::Add, Box::new(arg));
        }

        self.stack.push(entry);
        Ok(())
    }

    fn store(&mut self, idx: u8) -> JResult<Option<AST>> {
        let val = self.pop_stack()?;
        let ty = val.ty();
        let idx = usize::from(idx);
        let ident = match &ty {
            Type::Boolean => format!("bo{}", idx),
            Type::Byte => format!("by{}", idx),
            Type::Char => format!("ch{}", idx),
            Type::Short => format!("sh{}", idx),
            Type::Int => format!("i{}", idx),
            Type::Float => format!("f{}", idx),
            Type::Double => format!("d{}", idx),
            Type::Long => format!("l{}", idx),
            Type::ClassName(class) if class == "java/lang/String" => format!("s{}", idx),
            Type::ClassName(_) => format!("obj{}", idx),
            Type::Reference(_) => format!("arr{}", idx),
            Type::Void => panic!("attempted to store variable of type void"),
        };

        Ok(Some(
            match self
                .local_variables
                .insert(idx, StackEntry::Ident(ident.clone(), ty.clone()))
            {
                Some(..) => AST::ReAssignment {
                    var: Box::new(AST::Ident(ident, ty)),
                    val: val.into(),
                },
                None => AST::Assignment(ty, ident, val.into()),
            },
        ))
    }

    fn cast(&mut self, ty: Type) -> JResult<()> {
        let val = self.pop_stack()?;
        self.stack.push(StackEntry::Cast(ty, Box::new(val)));
        Ok(())
    }

    fn load(&mut self, idx: u8) {
        self.stack.push(
            self.local_variables
                .get(&usize::from(idx))
                .expect("expected local variable to exist")
                .clone(),
        );
    }

    fn binary_op(&mut self, op: BinaryOp) -> JResult<()> {
        let val2 = self.pop_stack()?;
        let val1 = self.pop_stack()?;
        self.stack
            .push(StackEntry::BinaryOp(Box::new(val1), op, Box::new(val2)));
        Ok(())
    }

    fn pop_stack(&mut self) -> JResult<StackEntry> {
        self.stack.pop().ok_or(ParseError::EmptyStack)
    }
}
