use core::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops;
use std::rc::Rc;
use std::sync::Arc;

use crate::bytecode::ByteCode;
use crate::common::Span;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    Object(Object),
}

impl Value {
    pub fn new_object(obj: Object) -> Self {
        Self::Object(obj)
    }

    pub fn try_into_number(self) -> Result<f64, Self> {
        if let Self::Number(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_bool(self) -> Result<bool, Self> {
        if let Self::Bool(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_string(self) -> Result<InternedString, Self> {
        if let Self::Object(Object::String(s)) = self {
            return Ok(s.clone());
        }

        Err(self)
    }

    pub fn try_to_string(&self) -> Option<InternedString> {
        if let Self::Object(Object::String(s)) = self {
            return Some(s.clone());
        }

        None
    }

    pub fn try_to_function(&self) -> Option<Rc<ObjFunction>> {
        if let Self::Object(Object::Function(fund)) = self {
            return Some(Rc::clone(fund));
        }

        None
    }

    pub fn try_to_class(&self) -> Option<Rc<ObjClass>> {
        if let Self::Object(Object::Class(cls)) = self {
            return Some(Rc::clone(cls));
        }

        None
    }

    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Object(o) => write!(f, "{}", o),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Object(o) => write!(f, "{:?}", o),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    String(InternedString),
    Function(Rc<ObjFunction>),
    NativeFn(Rc<NativeFn>),
    Closure(Rc<ObjClosure>),
    Upvalue(Rc<RefCell<ObjUpvalue>>),
    Class(Rc<ObjClass>),
    Instance(Rc<ObjInstance>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", **s),
            Object::Function(fun) => write!(f, "{}", fun),
            Object::NativeFn(_) => write!(f, "<native fun>"),
            Object::Closure(closure) => write!(f, "{}", closure),
            Object::Upvalue(upvalue) => write!(f, "{}", RefCell::borrow(upvalue)),
            Object::Class(cls) => write!(f, "{}", cls),
            Object::Instance(inst) => write!(f, "{}", inst),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternedString(Arc<String>);

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{:p}] {:?}", self.0, self.0)
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.0)
    }
}

impl InternedString {
    pub fn new(s: String) -> Self {
        Self(Arc::new(s))
    }
}

impl ops::Deref for InternedString {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Borrow<String> for InternedString {
    fn borrow(&self) -> &String {
        &self.0
    }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for InternedString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<InternedString> for String {
    fn from(s: InternedString) -> Self {
        s.0.to_string()
    }
}

impl From<&InternedString> for String {
    fn from(s: &InternedString) -> Self {
        s.0.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: usize,
    pub name: InternedString,
    pub bytecode: Rc<[u8]>,
    pub spans: Rc<HashMap<usize, Span>>,
    pub constants: Rc<[Value]>,
    pub upvalues_count: usize,
}

impl ObjFunction {
    pub fn new(
        name: InternedString,
        bytecode: ByteCode,
        constants: Rc<[Value]>,
        arity: usize,
        upvalues_count: usize,
    ) -> Self {
        Self {
            arity,
            name,
            bytecode: bytecode.code.into(),
            spans: Rc::new(bytecode.spans),
            constants,
            upvalues_count,
        }
    }
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fun {}>", self.name)
    }
}

pub type NativeFn = fn(u8, &[Value]) -> Value;

#[derive(Debug, Clone, PartialEq)]
pub struct ObjClosure {
    pub fun: Rc<ObjFunction>,
    pub upvalues: Vec<Rc<RefCell<ObjUpvalue>>>,
}

impl ObjClosure {
    pub fn new(fun: Rc<ObjFunction>, upvalues: Vec<Rc<RefCell<ObjUpvalue>>>) -> Self {
        Self { fun, upvalues }
    }
}

impl fmt::Display for ObjClosure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<closure {}>", self.fun.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ObjUpvalue {
    Open(usize),
    Closed(Value),
}

impl fmt::Display for ObjUpvalue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ObjUpvalue::Open(idx) => write!(f, "<upvalue (open) {}>", idx),
            ObjUpvalue::Closed(v) => write!(f, "<upvalue (closed) {}>", v),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjClass {
    pub name: InternedString,
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}

impl ObjClass {
    pub fn new(name: InternedString) -> Self {
        Self { name }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjInstance {
    pub class: Rc<ObjClass>,
    pub fields: HashMap<InternedString, Value>,
}

impl ObjInstance {
    pub fn new(class: Rc<ObjClass>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

impl fmt::Display for ObjInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<instance {}>", self.class.name)
    }
}
