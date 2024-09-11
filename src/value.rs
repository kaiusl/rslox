use core::fmt;
use std::borrow::{Borrow, Cow};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops;
use std::rc::{Rc, Weak};
use std::sync::Arc;

use crate::bytecode::ByteCode;
use crate::common::Span;
use crate::vm::{self, BuildHasher};

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

    pub fn new_class(cls: ObjClass) -> Self {
        Self::new_object(Object::Class(Rc::new(RefCell::new(cls))))
    }

    pub fn new_instance(inst: ObjInstance) -> Self {
        Self::new_object(Object::Instance(Rc::new(RefCell::new(inst))))
    }

    pub fn new_closure(closure: ObjClosure) -> Self {
        Self::new_object(Object::Closure(Rc::new(closure)))
    }

    pub fn new_bound_method(bm: ObjBoundMethod) -> Self {
        Self::new_object(Object::BoundMethod(Rc::new(bm)))
    }

    pub fn try_as_string(&self) -> Option<&InternedString> {
        if let Self::Object(Object::String(s)) = self {
            return Some(s);
        }

        None
    }

    pub fn try_to_string(&self) -> Option<InternedString> {
        self.try_as_string().cloned()
    }

    pub fn try_into_string(self) -> Option<InternedString> {
        if let Self::Object(Object::String(s)) = self {
            return Some(s);
        }

        None
    }

    pub fn try_as_function(&self) -> Option<&Rc<ObjFunction>> {
        if let Self::Object(Object::Function(fund)) = self {
            return Some(fund);
        }

        None
    }

    pub fn try_to_function(&self) -> Option<Rc<ObjFunction>> {
        self.try_as_function().cloned()
    }

    pub fn try_into_function(self) -> Option<Rc<ObjFunction>> {
        if let Self::Object(Object::Function(fund)) = self {
            return Some(fund);
        }

        None
    }

    pub fn is_closure(&self) -> bool {
        matches!(self, Self::Object(Object::Closure(_)))
    }

    pub fn try_as_closure(&self) -> Option<Cow<'_, Rc<ObjClosure>>> {
        match self {
            Self::Object(Object::Closure(closure)) => return Some(Cow::Borrowed(closure)),
            Self::Object(Object::WeakClosure(closure)) => {
                return Some(Cow::Owned(closure.upgrade().unwrap()))
            }
            _ => (),
        }

        None
    }

    pub fn try_to_closure(&self) -> Option<Rc<ObjClosure>> {
        self.try_as_closure().map(|a| match a {
            Cow::Borrowed(a) => a.clone(),
            Cow::Owned(a) => a,
        })
    }

    pub fn try_into_closure(self) -> Option<Rc<ObjClosure>> {
        match self {
            Self::Object(Object::Closure(closure)) => Some(closure),
            Self::Object(Object::WeakClosure(closure)) => Some(closure.upgrade().unwrap()),
            _ => None,
        }
    }

    pub fn try_as_class(&self) -> Option<&Rc<RefCell<ObjClass>>> {
        if let Self::Object(Object::Class(cls)) = self {
            return Some(cls);
        }

        None
    }

    pub fn try_to_class(&self) -> Option<Rc<RefCell<ObjClass>>> {
        self.try_as_class().cloned()
    }

    pub fn try_into_class(self) -> Option<Rc<RefCell<ObjClass>>> {
        if let Self::Object(Object::Class(cls)) = self {
            return Some(cls);
        }

        None
    }

    pub fn try_as_instance(&self) -> Option<Cow<'_, Rc<RefCell<ObjInstance>>>> {
        match self {
            Self::Object(Object::Instance(inst)) => return Some(Cow::Borrowed(inst)),
            Self::Object(Object::WeakInstance(inst)) => {
                return Some(Cow::Owned(inst.upgrade().unwrap()))
            }
            _ => (),
        }

        None
    }

    pub fn try_to_instance(&self) -> Option<Rc<RefCell<ObjInstance>>> {
        self.try_as_instance().map(|a| match a {
            Cow::Borrowed(a) => a.clone(),
            Cow::Owned(a) => a,
        })
    }

    pub fn try_into_instance(self) -> Option<Rc<RefCell<ObjInstance>>> {
        match self {
            Self::Object(Object::Instance(inst)) => Some(inst),
            Self::Object(Object::WeakInstance(inst)) => Some(inst.upgrade().unwrap()),
            _ => None,
        }
    }

    pub fn is_falsey(&self) -> bool {
        matches!(self, Value::Nil | Value::Bool(false))
    }

    pub fn to_weak(&self) -> Self {
        match self {
            Value::Object(o) => match o {
                //Object::Function(fun) => Self::WeakObject(WeakObject::Function(Rc::downgrade(fun))),
                Object::Closure(closure) => {
                    Self::Object(Object::WeakClosure(Rc::downgrade(closure)))
                }
                // Object::Upvalue(upvalue) => {
                //     Self::WeakObject(WeakObject::Upvalue(Rc::downgrade(upvalue)))
                // }
                // Object::Class(cls) => Self::WeakObject(WeakObject::Class(Rc::downgrade(cls))),
                Object::Instance(inst) => Self::Object(Object::WeakInstance(Rc::downgrade(inst))),
                // Object::BoundMethod(bm) => {
                //     Self::WeakObject(WeakObject::BoundMethod(Rc::downgrade(bm)))
                // }
                _ => panic!("cannot convert {:?} to weak", o),
            },
            _ => self.clone(),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Object(o) => write!(f, "{}", o),
            //Value::WeakObject(o) => write!(f, "{}", o),
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
            //Value::WeakObject(o) => write!(f, "{:?}", o),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Object {
    String(InternedString),
    Function(Rc<ObjFunction>),
    NativeFn(Rc<NativeFn>),
    Closure(Rc<ObjClosure>),
    WeakClosure(Weak<ObjClosure>),
    Upvalue(Rc<RefCell<ObjUpvalue>>), // RefCell because we need to close it
    Class(Rc<RefCell<ObjClass>>), // RefCell because we need to add all the methods after the class has been created
    Instance(Rc<RefCell<ObjInstance>>), // RefCell because we need to add properties to it
    WeakInstance(Weak<RefCell<ObjInstance>>),
    BoundMethod(Rc<ObjBoundMethod>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", **s),
            Object::Function(fun) => write!(f, "{}", fun),
            Object::NativeFn(_) => write!(f, "<native fun>"),
            Object::Closure(closure) => write!(f, "{}", closure),
            Object::WeakClosure(closure) => write!(f, "{}", closure.upgrade().unwrap()),
            Object::Upvalue(upvalue) => write!(f, "{}", RefCell::borrow(upvalue)),
            Object::Class(cls) => write!(f, "{}", RefCell::borrow(cls)),
            Object::Instance(inst) => write!(f, "{}", RefCell::borrow(inst)),
            Object::WeakInstance(inst) => {
                write!(f, "{}", RefCell::borrow(&inst.upgrade().unwrap()))
            }
            Object::BoundMethod(bm) => write!(f, "{}", bm),
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Function(l0), Self::Function(r0)) => l0 == r0,
            (Self::NativeFn(l0), Self::NativeFn(r0)) => l0 == r0,
            (Self::Closure(l0), Self::Closure(r0)) => l0 == r0,
            (Self::WeakClosure(l0), Self::WeakClosure(r0)) => l0.upgrade() == r0.upgrade(),
            (Self::Upvalue(l0), Self::Upvalue(r0)) => l0 == r0,
            (Self::Class(l0), Self::Class(r0)) => l0 == r0,
            (Self::Instance(l0), Self::Instance(r0)) => l0 == r0,
            (Self::WeakInstance(l0), Self::WeakInstance(r0)) => l0.upgrade() == r0.upgrade(),
            (Self::BoundMethod(l0), Self::BoundMethod(r0)) => l0 == r0,
            _ => false,
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
    pub spans: Rc<HashMap<usize, Span, BuildHasher>>,
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
    pub methods: HashMap<InternedString, Value, BuildHasher>,
}

impl fmt::Display for ObjClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}

impl ObjClass {
    pub fn new(name: InternedString) -> Self {
        Self {
            name,
            methods: HashMap::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjInstance {
    pub class: Rc<RefCell<ObjClass>>,
    pub properties: HashMap<InternedString, Value, BuildHasher>,
}

impl ObjInstance {
    pub fn new(class: Rc<RefCell<ObjClass>>) -> Self {
        Self {
            class,
            properties: HashMap::default(),
        }
    }
}

impl fmt::Display for ObjInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<instance {}>", RefCell::borrow(&self.class).name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ObjBoundMethod {
    pub receiver: Value,
    pub method: Rc<ObjClosure>,
}

impl fmt::Display for ObjBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.method)
    }
}

impl ObjBoundMethod {
    pub fn new(receiver: Value, method: Rc<ObjClosure>) -> Self {
        Self {
            // A strong reference can cause cycles if the bound method is set as a field to the same instance
            // It doesn't seem to break anything if we set it always as weak ref.
            receiver: receiver.to_weak(),
            method,
        }
    }
}
