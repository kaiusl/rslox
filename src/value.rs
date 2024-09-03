use core::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::ops;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    Object(Rc<RefCell<Object>>),
}

impl Value {
    pub fn new_object(obj: Object) -> Self {
        Self::Object(Rc::new(RefCell::new(obj)))
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
            Value::Object(o) => write!(f, "{}", RefCell::borrow(o)),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Object(o) => write!(f, "[{:p}] {:?}", o, RefCell::borrow(o)),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    String(InternedString),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", **s),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternedString(Rc<String>);

impl fmt::Debug for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{:p}] {:?}", self.0, self.0)
    }
}

impl InternedString {
    pub fn new(s: String) -> Self {
        Self(Rc::new(s))
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
