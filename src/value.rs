use core::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::ops;
use std::rc::Rc;
use std::sync::Arc;

use crate::bytecode::ByteCode;

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

    #[must_use]
    pub fn try_into_number(self) -> Result<f64, Self> {
        if let Self::Number(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    #[must_use]
    pub fn try_into_bool(self) -> Result<bool, Self> {
        if let Self::Bool(v) = self {
            Ok(v)
        } else {
            Err(self)
        }
    }

    pub fn try_into_string(self) -> Result<InternedString, Self> {
        if let Self::Object(o) = &self {
            if let Object::String(s) = o {
                return Ok(s.clone());
            }
        }

        Err(self)
    }

    pub fn try_to_string(&self) -> Option<InternedString> {
        if let Self::Object(o) = self {
            if let Object::String(s) = o {
                return Some(s.clone());
            }
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
    NativeFn(Rc<NativeFn>)
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", **s),
            Object::Function(fun) => write!(f, "{}", fun),
            Object::NativeFn(_) => write!(f, "<native fun>"),
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

#[derive(Debug, Clone, PartialEq)]
pub struct ObjFunction {
    pub arity: usize,
    pub name: InternedString,
    pub bytecode: ByteCode,
}

impl fmt::Display for ObjFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<fun {}>", self.name)
    }
}

pub type NativeFn = fn(u8, &[Value]) -> Value;