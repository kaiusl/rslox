use core::fmt;
use std::borrow::{self, Cow};
use std::cell::{Cell, RefCell};
use std::hash::BuildHasher as _;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;
use std::{mem, ops, ptr};

use hashbrown::{HashMap, HashSet};

use crate::bytecode::ByteCode;
use crate::common::Span;
use crate::vm::BuildHasher;

use crate::vm::CallFrame;

// Wrapper around `ValueInner` to force creating new Values through Self::new_..
// functions which properly handle adding objects to Gc.
#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub struct Value(ValueInner);

impl Value {
    pub fn new_number(n: f64) -> Self {
        Self(ValueInner::Number(n))
    }

    pub fn new_bool(b: bool) -> Self {
        Self(ValueInner::Bool(b))
    }

    pub fn new_nil() -> Self {
        Self(ValueInner::Nil)
    }

    pub fn new_string(s: InternedString) -> Self {
        Self(ValueInner::Object(Object::String(s)))
    }

    pub fn new_class(cls: ObjClass, gc: &mut Gc) -> Self {
        let obj = gc.new_object(RefCell::new(cls));
        Self(ValueInner::Object(obj))
    }

    pub fn new_instance(inst: ObjInstance, gc: &mut Gc) -> Self {
        let obj = gc.new_object(RefCell::new(inst));
        Self(ValueInner::Object(obj))
    }

    pub fn new_closure(closure: ObjClosure, gc: &mut Gc) -> Self {
        let obj = gc.new_object(closure);
        Self(ValueInner::Object(obj))
    }

    pub fn new_bound_method(bm: ObjBoundMethod, gc: &mut Gc) -> Self {
        let obj = gc.new_object(bm);
        Self(ValueInner::Object(obj))
    }

    pub fn new_native_fn(fun: NativeFn, gc: &mut Gc) -> Self {
        let obj = gc.new_object(fun);
        Self(ValueInner::Object(obj))
    }

    pub fn new_function(fun: ObjFunction, gc: &mut Gc) -> Self {
        let obj = gc.new_object(fun);
        Self(ValueInner::Object(obj))
    }

    pub fn new_upvalue(upvalue: ObjUpvalue, gc: &mut Gc) -> Self {
        let obj = gc.new_object(RefCell::new(upvalue));
        Self(ValueInner::Object(obj))
    }

    pub fn into_inner(self) -> ValueInner {
        self.0
    }

    pub fn as_inner(&self) -> &ValueInner {
        &self.0
    }
}

impl ops::Deref for Value {
    type Target = ValueInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Copy, PartialEq)]
pub enum ValueInner {
    Number(f64),
    Bool(bool),
    Nil,
    Object(Object),
}

impl Clone for ValueInner {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl ValueInner {
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

    pub fn try_as_function(&self) -> Option<&GcObj<ObjFunction>> {
        if let Self::Object(Object::Function(fund)) = self {
            return Some(fund);
        }

        None
    }

    pub fn try_to_function(&self) -> Option<GcObj<ObjFunction>> {
        self.try_as_function().cloned()
    }

    pub fn try_into_function(self) -> Option<GcObj<ObjFunction>> {
        if let Self::Object(Object::Function(fund)) = self {
            return Some(fund);
        }

        None
    }

    pub fn is_closure(&self) -> bool {
        matches!(self, Self::Object(Object::Closure(_)))
    }

    pub fn try_as_closure(&self) -> Option<Cow<'_, GcObj<ObjClosure>>> {
        if let Self::Object(Object::Closure(closure)) = self {
            return Some(Cow::Borrowed(closure));
        }

        None
    }

    pub fn try_to_closure(&self) -> Option<GcObj<ObjClosure>> {
        self.try_as_closure().map(|a| match a {
            Cow::Borrowed(a) => *a,
            Cow::Owned(a) => a,
        })
    }

    pub fn try_into_closure(self) -> Option<GcObj<ObjClosure>> {
        match self {
            Self::Object(Object::Closure(closure)) => Some(closure),
            _ => None,
        }
    }

    pub fn try_as_class(&self) -> Option<&GcObj<RefCell<ObjClass>>> {
        if let Self::Object(Object::Class(cls)) = self {
            return Some(cls);
        }

        None
    }

    pub fn try_to_class(&self) -> Option<GcObj<RefCell<ObjClass>>> {
        self.try_as_class().cloned()
    }

    pub fn try_into_class(self) -> Option<GcObj<RefCell<ObjClass>>> {
        if let Self::Object(Object::Class(cls)) = self {
            return Some(cls);
        }

        None
    }

    pub fn try_as_instance(&self) -> Option<Cow<'_, GcObj<RefCell<ObjInstance>>>> {
        if let Self::Object(Object::Instance(inst)) = self {
            return Some(Cow::Borrowed(inst));
        }

        None
    }

    pub fn try_to_instance(&self) -> Option<GcObj<RefCell<ObjInstance>>> {
        self.try_as_instance().map(|a| match a {
            Cow::Borrowed(a) => *a,
            Cow::Owned(a) => a,
        })
    }

    pub fn try_into_instance(self) -> Option<GcObj<RefCell<ObjInstance>>> {
        match self {
            Self::Object(Object::Instance(inst)) => Some(inst),
            _ => None,
        }
    }

    pub fn is_falsey(&self) -> bool {
        matches!(self, ValueInner::Nil | ValueInner::Bool(false))
    }
}

impl fmt::Display for ValueInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueInner::Number(n) => write!(f, "{}", n),
            ValueInner::Bool(b) => write!(f, "{}", b),
            ValueInner::Nil => write!(f, "nil"),
            ValueInner::Object(o) => write!(f, "{}", o),
        }
    }
}

impl fmt::Debug for ValueInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueInner::Number(n) => write!(f, "{}", n),
            ValueInner::Bool(b) => write!(f, "{}", b),
            ValueInner::Nil => write!(f, "nil"),
            ValueInner::Object(o) => write!(f, "{:?}", o),
        }
    }
}

#[derive(Copy, Debug)]
pub enum Object {
    String(InternedString),
    Function(GcObj<ObjFunction>),
    NativeFn(GcObj<NativeFn>),
    Closure(GcObj<ObjClosure>),
    Upvalue(GcObj<RefCell<ObjUpvalue>>), // RefCell because we need to close it
    Class(GcObj<RefCell<ObjClass>>), // RefCell because we need to add all the methods after the class has been created
    Instance(GcObj<RefCell<ObjInstance>>), // RefCell because we need to add properties to it
    BoundMethod(GcObj<ObjBoundMethod>),
}

impl From<GcObj<InternedStringCore>> for Object {
    fn from(value: GcObj<InternedStringCore>) -> Self {
        Object::String(InternedString(value))
    }
}

impl From<GcObj<ObjFunction>> for Object {
    fn from(fun: GcObj<ObjFunction>) -> Self {
        Object::Function(fun)
    }
}

impl From<GcObj<NativeFn>> for Object {
    fn from(fun: GcObj<NativeFn>) -> Self {
        Object::NativeFn(fun)
    }
}

impl From<GcObj<ObjClosure>> for Object {
    fn from(fun: GcObj<ObjClosure>) -> Self {
        Object::Closure(fun)
    }
}

impl From<GcObj<RefCell<ObjUpvalue>>> for Object {
    fn from(upvalue: GcObj<RefCell<ObjUpvalue>>) -> Self {
        Object::Upvalue(upvalue)
    }
}

impl From<GcObj<RefCell<ObjClass>>> for Object {
    fn from(cls: GcObj<RefCell<ObjClass>>) -> Self {
        Object::Class(cls)
    }
}

impl From<GcObj<RefCell<ObjInstance>>> for Object {
    fn from(inst: GcObj<RefCell<ObjInstance>>) -> Self {
        Object::Instance(inst)
    }
}

impl From<GcObj<ObjBoundMethod>> for Object {
    fn from(bm: GcObj<ObjBoundMethod>) -> Self {
        Object::BoundMethod(bm)
    }
}

impl Object {
    fn alloc_addr(&self) -> usize {
        match self {
            Object::String(s) => s.0.alloc_addr(),
            Object::Function(f) => f.alloc_addr(),
            Object::NativeFn(f) => f.alloc_addr(),
            Object::Closure(o) => o.alloc_addr(),
            Object::Upvalue(o) => o.alloc_addr(),
            Object::Class(o) => o.alloc_addr(),
            Object::Instance(o) => o.alloc_addr(),
            Object::BoundMethod(o) => o.alloc_addr(),
        }
    }
}

impl Clone for Object {
    #[inline]
    fn clone(&self) -> Self {
        *self
    }
}

impl fmt::Pointer for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#x}", self.alloc_addr())
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::String(s) => write!(f, "{}", **s),
            Object::Function(fun) => write!(f, "{}", **fun),
            Object::NativeFn(_) => write!(f, "<native fun>"),
            Object::Closure(closure) => write!(f, "{}", **closure),
            Object::Upvalue(upvalue) => write!(f, "{}", RefCell::borrow(upvalue)),
            Object::Class(cls) => write!(f, "{}", RefCell::borrow(cls)),
            Object::Instance(inst) => write!(f, "{}", RefCell::borrow(inst)),
            Object::BoundMethod(bm) => write!(f, "{}", **bm),
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
            (Self::Upvalue(l0), Self::Upvalue(r0)) => l0 == r0,
            (Self::Class(l0), Self::Class(r0)) => l0 == r0,
            (Self::Instance(l0), Self::Instance(r0)) => l0 == r0,
            (Self::BoundMethod(l0), Self::BoundMethod(r0)) => l0 == r0,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct StringInterner {
    strings: HashSet<InternedString, BuildHasher>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: HashSet::default(),
        }
    }

    pub fn intern(&mut self, s: impl Into<String> + AsRef<str>, gc: &mut Gc) -> InternedString {
        let s_ref = s.as_ref();
        match self.strings.get(s_ref) {
            Some(existing) => *existing,
            None => {
                let new = InternedStringCore::new(s.into());
                let obj = gc.new_object_inner(new);
                let new = InternedString(obj);
                self.strings.insert(new);
                new
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Copy)]
#[repr(transparent)]
pub struct InternedString(GcObj<InternedStringCore>);

impl std::hash::Hash for InternedString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let inner: &InternedStringCore = &self.0;
        inner.hash(state)
    }
}

#[derive(Debug)]
struct InternedStringCore {
    hash: u64,
    value: String,
}

impl InternedStringCore {
    fn new(value: String) -> Self {
        let hasher = BuildHasher::default();
        let hash = hasher.hash_one(&value);
        Self { hash, value }
    }
}

impl PartialEq for InternedStringCore {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl Eq for InternedStringCore {}

impl std::hash::Hash for InternedStringCore {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl fmt::Display for InternedStringCore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

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
    pub fn get_hash(&self) -> u64 {
        #[cfg(debug_assertions)]
        {
            let hasher = BuildHasher::default();
            let new_hash = hasher.hash_one(&self.0.value);
            assert_eq!(self.0.hash, new_hash, "cached hash doesn't match");
        }
        self.0.hash
    }
}

impl ops::Deref for InternedString {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.0.value
    }
}

impl borrow::Borrow<String> for InternedString {
    fn borrow(&self) -> &String {
        &self.0.value
    }
}

impl borrow::Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        &self.0.value
    }
}

impl AsRef<str> for InternedString {
    fn as_ref(&self) -> &str {
        &self.0.value
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
    pub fun: GcObj<ObjFunction>,
    pub upvalues: Vec<GcObj<RefCell<ObjUpvalue>>>,
}

impl ObjClosure {
    pub fn new(fun: GcObj<ObjFunction>, upvalues: Vec<GcObj<RefCell<ObjUpvalue>>>) -> Self {
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
    pub class: GcObj<RefCell<ObjClass>>,
    pub properties: HashMap<InternedString, Value, BuildHasher>,
}

impl ObjInstance {
    pub fn new(class: GcObj<RefCell<ObjClass>>) -> Self {
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
    pub method: GcObj<ObjClosure>,
}

impl fmt::Display for ObjBoundMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.method)
    }
}

impl ObjBoundMethod {
    pub fn new(receiver: Value, method: GcObj<ObjClosure>) -> Self {
        Self {
            // A strong reference can cause cycles if the bound method is set as a field to the same instance
            // It doesn't seem to break anything if we set it always as weak ref.
            receiver,
            method,
        }
    }
}

pub use gc::{Gc, GcObj};

mod gc {
    use super::*;

    /// Helper functions on Object that are only relevant to the garbage collector
    impl Object {
        fn set_mark(&self) {
            #[cfg(feature = "debug_gc")]
            {
                println!("gc: marked [{:#x}] {}", self.alloc_addr(), self);
            }
            match self {
                Object::String(s) => s.0.set_mark(),
                Object::Function(fun) => fun.set_mark(),
                Object::NativeFn(fun) => fun.set_mark(),
                Object::Closure(closure) => closure.set_mark(),
                Object::Upvalue(upvalue) => upvalue.set_mark(),
                Object::Class(cls) => cls.set_mark(),
                Object::Instance(inst) => inst.set_mark(),
                Object::BoundMethod(bm) => bm.set_mark(),
            }
        }

        fn is_marked(&self) -> bool {
            match self {
                Object::String(s) => s.0.is_marked(),
                Object::Function(f) => f.is_marked(),
                Object::NativeFn(f) => f.is_marked(),
                Object::Closure(o) => o.is_marked(),
                Object::Upvalue(o) => o.is_marked(),
                Object::Class(o) => o.is_marked(),
                Object::Instance(o) => o.is_marked(),
                Object::BoundMethod(o) => o.is_marked(),
            }
        }

        fn clean_mark(&self) {
            #[cfg(feature = "debug_gc")]
            {
                println!("gc: cleaned mark [{:#x}] {}", self.alloc_addr(), self);
            }
            match self {
                Object::String(s) => s.0.clean_mark(),
                Object::Function(f) => f.clean_mark(),
                Object::NativeFn(f) => f.clean_mark(),
                Object::Closure(o) => o.clean_mark(),
                Object::Upvalue(o) => o.clean_mark(),
                Object::Class(o) => o.clean_mark(),
                Object::Instance(o) => o.clean_mark(),
                Object::BoundMethod(o) => o.clean_mark(),
            }
        }

        /// Frees the underlying allocation of the object and returns the number of bytes that were freed.
        ///
        /// # Safety
        ///
        /// The caller must ensure that any of the Objects-s that point to this allocation are never used again.
        unsafe fn free(self) -> usize {
            #[cfg(feature = "debug_gc")]
            {
                println!("gc: free [{:#x}] {}", self.alloc_addr(), self);
            }
            // SAFETY: Safety requirements of inner calls are forwarded to this functions caller
            unsafe {
                match self {
                    Object::String(s) => s.0.free(),
                    Object::Function(f) => f.free(),
                    Object::NativeFn(f) => f.free(),
                    Object::Closure(o) => o.free(),
                    Object::Upvalue(o) => o.free(),
                    Object::Class(o) => o.free(),
                    Object::Instance(o) => o.free(),
                    Object::BoundMethod(o) => o.free(),
                }
            }
        }

        fn alloc_size(&self) -> usize {
            match self {
                Object::String(s) => s.0.alloc_size(),
                Object::Function(f) => f.alloc_size(),
                Object::NativeFn(f) => f.alloc_size(),
                Object::Closure(o) => o.alloc_size(),
                Object::Upvalue(o) => o.alloc_size(),
                Object::Class(o) => o.alloc_size(),
                Object::Instance(o) => o.alloc_size(),
                Object::BoundMethod(o) => o.alloc_size(),
            }
        }

        /// # Safety
        ///
        /// Caller must ensure that `obj` is not already in the linked list ob objects, eg cycles are not allowed.
        /// Otherwise gc may access already freed memory during the sweep or get stuck in infinite loop.
        unsafe fn set_next_obj(&self, obj: Option<Object>) {
            unsafe {
                match self {
                    Object::String(s) => s.0.set_next_obj(obj),
                    Object::Function(f) => f.set_next_obj(obj),
                    Object::NativeFn(f) => f.set_next_obj(obj),
                    Object::Closure(o) => o.set_next_obj(obj),
                    Object::Upvalue(o) => o.set_next_obj(obj),
                    Object::Class(o) => o.set_next_obj(obj),
                    Object::Instance(o) => o.set_next_obj(obj),
                    Object::BoundMethod(o) => o.set_next_obj(obj),
                }
            }
        }

        fn next_obj(&self) -> Option<Object> {
            match self {
                Object::String(s) => s.0.next_obj(),
                Object::Function(f) => f.next_obj(),
                Object::NativeFn(f) => f.next_obj(),
                Object::Closure(o) => o.next_obj(),
                Object::Upvalue(o) => o.next_obj(),
                Object::Class(o) => o.next_obj(),
                Object::Instance(o) => o.next_obj(),
                Object::BoundMethod(o) => o.next_obj(),
            }
        }
    }

    #[derive(Debug)]
    pub struct Gc {
        gray_stack: Vec<Object>,
        bytes_allocated: usize,
        next_gc: usize,
        first_object: Option<Object>,
        last_object: Option<Object>,
    }

    impl Gc {
        const FIRST_GC_BYTES: usize = 1024 * 1024;
        const GROWTH_FACTOR: usize = 2;

        pub fn new() -> Self {
            Self {
                gray_stack: Vec::new(),
                first_object: None,
                last_object: None,
                bytes_allocated: 0,
                next_gc: Self::FIRST_GC_BYTES,
            }
        }

        pub fn new_object<T>(&mut self, value: T) -> Object
        where
            GcObj<T>: Into<Object>,
        {
            let obj = GcObj::new(value).into();
            self.add_object(obj);
            obj
        }

        pub fn new_object_inner<T>(&mut self, value: T) -> GcObj<T>
        where
            GcObj<T>: Into<Object>,
        {
            let obj = GcObj::new(value);
            self.add_object(obj.into());
            obj
        }

        /// Adds an object to the responsibility of this garbage collector.
        fn add_object(&mut self, obj: Object) {
            match self.last_object {
                None => {
                    self.first_object = Some(obj);
                    self.last_object = Some(obj);
                }
                Some(ref last) => {
                    unsafe { last.set_next_obj(Some(obj)) };
                    self.last_object = Some(obj);
                }
            }

            self.bytes_allocated += obj.alloc_size();

            #[cfg(feature = "debug_gc")]
            {
                println!(
                    "gc: added [{:p}] {}, next: {}",
                    obj,
                    obj,
                    obj.next_obj().is_none()
                );
                println!(
                    "gc: first: {}, last: {}",
                    self.first_object.unwrap(),
                    self.last_object.unwrap()
                );
            }
        }

        pub fn should_collect(&self) -> bool {
            self.bytes_allocated > self.next_gc
        }

        /// # Safety
        ///
        /// All of the roots must be marked and added to the gray stack of self before calling this method.
        ///
        /// Use self.mark_.. methods to mark values/objects etc.
        pub unsafe fn collect(&mut self) {
            #[cfg(feature = "debug_gc")]
            {
                println!(
                    "gc: collect. allocated: {:.1} kB",
                    self.bytes_allocated as f64 / 1024.0
                );
            }
            self.trace_references();
            unsafe { self.sweep() };

            self.next_gc = (self.bytes_allocated * Self::GROWTH_FACTOR).max(Self::FIRST_GC_BYTES);

            #[cfg(feature = "debug_gc")]
            {
                println!(
                    "gc: collect -- done: {:.1} kB",
                    self.bytes_allocated as f64 / 1024.0
                );
            }
        }

        pub fn mark_call_frame(&mut self, frame: &CallFrame) {
            let closure = Object::Closure(frame.closure);
            self.mark_obj(closure);
        }

        pub fn mark_value(&mut self, v: &Value) {
            if let ValueInner::Object(obj) = v.as_inner() {
                self.mark_obj(*obj);
            }
        }

        pub fn mark_obj(&mut self, obj: Object) {
            if obj.is_marked() {
                return;
            }
            obj.set_mark();
            self.gray_stack.push(obj);
        }

        fn trace_references(&mut self) {
            #[cfg(feature = "debug_gc")]
            {
                println!("gc: tracing references");
            }
            while let Some(obj) = self.gray_stack.pop() {
                self.blacken_obj(obj);
            }

            #[cfg(feature = "debug_gc")]
            {
                println!("gc: tracing references -- done");
            }
        }

        fn blacken_obj(&mut self, obj: Object) {
            #[cfg(feature = "debug_gc")]
            {
                println!("gc: blackened [{:p}] {}", obj, obj);
            }
            match obj {
                Object::String(_) => {}
                Object::NativeFn(_) => {}
                Object::Function(fun) => {
                    let name = Object::String(fun.name);
                    self.mark_obj(name);
                    for it in fun.constants.iter() {
                        self.mark_value(it);
                    }
                }
                Object::Closure(c) => {
                    let fun = Object::Function(c.fun);
                    self.mark_obj(fun);
                    for it in c.upvalues.iter() {
                        let it = Object::Upvalue(*it);
                        self.mark_obj(it);
                    }
                }
                Object::Upvalue(o) => {
                    let o = o.borrow();
                    if let ObjUpvalue::Closed(obj) = o.deref() {
                        self.mark_value(obj);
                    }
                }
                Object::Class(c) => {
                    let c = c.borrow();
                    let name = Object::String(c.name);
                    self.mark_obj(name);
                    for it in c.methods.iter() {
                        it.0.0.set_mark();
                        self.mark_value(it.1);
                    }
                }
                Object::Instance(i) => {
                    let i = i.borrow();
                    let class = Object::Class(i.class);
                    self.mark_obj(class);
                    for it in i.properties.iter() {
                        it.0.0.set_mark();
                        self.mark_value(it.1);
                    }
                }
                Object::BoundMethod(bm) => {
                    self.mark_value(&bm.receiver);
                    let method = Object::Closure(bm.method);
                    self.mark_obj(method);
                }
            }
        }

        /// # Safety
        ///
        /// All reachable objects must be marked.
        unsafe fn sweep(&mut self) {
            let mut last_kept_obj: Option<Object> = None;
            let mut first_kept_obj: Option<Object> = None;
            let mut next_obj_to_check = self.first_object;
            while let Some(o) = next_obj_to_check {
                // println!(
                //     "gc: sweep [{:#x}] {}, next: {:#x}",
                //     o.alloc_addr(),
                //     o,
                //     o.next_obj().map(|o| o.alloc_addr()).unwrap_or(0)
                // );

                next_obj_to_check = o.next_obj();

                if !o.is_marked() {
                    // Free
                    let bytes = unsafe { o.free() };
                    self.bytes_allocated -= bytes;
                } else {
                    // Keep
                    if let Some(last_kept) = last_kept_obj {
                        unsafe { last_kept.set_next_obj(Some(o)) };
                    }

                    last_kept_obj = Some(o);
                    if first_kept_obj.is_none() {
                        first_kept_obj = Some(o);
                    }
                    o.clean_mark();
                }
            }

            self.first_object = first_kept_obj;
            self.last_object = last_kept_obj;
        }
    }

    impl Drop for Gc {
        fn drop(&mut self) {
            let mut next_obj_to_check = self.first_object;
            while let Some(o) = next_obj_to_check {
                // println!(
                //     "gc: sweep [{:#x}] {}, next: {:#x}",
                //     o.alloc_addr(),
                //     o,
                //     o.next_obj().map(|o| o.alloc_addr()).unwrap_or(0)
                // );

                next_obj_to_check = o.next_obj();
                unsafe { o.free() };
            }
        }
    }

    pub struct GcObj<T> {
        ptr: ptr::NonNull<GcBox<T>>,
        marker: PhantomData<GcBox<T>>,
    }

    impl<T> GcObj<T> {
        /// Create a dangling GcObj
        ///
        /// # Safety
        ///
        /// The returned GcObj must not be actually accessed
        pub unsafe fn dangling() -> Self {
            Self {
                ptr: ptr::NonNull::dangling(),
                marker: PhantomData,
            }
        }

        fn new(data: T) -> Self {
            let inner = GcBox {
                value: data,
                next_obj: Cell::new(None),
                is_marked: Cell::new(false),
            };
            let ptr = Box::into_raw(Box::new(inner));

            #[cfg(feature = "debug_gc")]
            println!("gc: created [{:p}]", ptr);
            GcObj {
                ptr: unsafe { ptr::NonNull::new_unchecked(ptr) },
                marker: PhantomData,
            }
        }

        /// # Safety
        ///
        /// Caller must ensure that `obj` is not already in the linked list ob objects, eg cycles are not allowed.
        /// Otherwise gc may access already freed memory during the sweep or get stuck in infinite loop.
        unsafe fn set_next_obj(&self, next_obj: Option<Object>) {
            self.as_inner().next_obj.set(next_obj);
        }

        fn next_obj(&self) -> Option<Object> {
            self.as_inner().next_obj.get()
        }

        #[inline(always)]
        fn as_inner(&self) -> &GcBox<T> {
            // SAFETY:
            //   First, the inner allocation can only be deallocated by self.free function.
            //   The caller of that function must ensure that any of the GcObj-s that point to this allocation are never used again.
            //   Thus when this function is called, the allocation must still be alive.
            //
            //   Second, we never give out mutable references to the inner allocation and thus it's always valid to create a immutable reference to it.
            unsafe { self.ptr.as_ref() }
        }

        fn set_mark(&self) {
            self.as_inner().is_marked.set(true);
        }

        fn is_marked(&self) -> bool {
            self.as_inner().is_marked.get()
        }

        fn clean_mark(&self) {
            self.as_inner().is_marked.set(false);
        }

        /// Frees the underlying allocation and returns the number of bytes that were freed.
        ///
        ///  # Safety
        ///
        /// The caller must ensure that any of the GcObj-s that point to this allocation are never used again.
        unsafe fn free(self) -> usize {
            let _ = unsafe { Box::from_raw(self.ptr.as_ptr()) };
            self.alloc_size()
        }

        const fn alloc_size(&self) -> usize {
            mem::size_of::<GcBox<T>>()
        }

        pub fn alloc_addr(&self) -> usize {
            self.ptr.as_ptr() as usize
        }
    }

    impl<T> ops::Deref for GcObj<T> {
        type Target = T;

        fn deref(&self) -> &T {
            &self.as_inner().value
        }
    }

    impl<T> Clone for GcObj<T> {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl<T> Copy for GcObj<T> {}

    impl<T> fmt::Debug for GcObj<T>
    where
        T: fmt::Debug,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut o = f.debug_struct("GcObj");
            o.field("ptr", &self.ptr);
            o.field("value", &self);
            o.finish()
        }
    }

    impl<T> fmt::Pointer for GcObj<T> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{:p}", self.ptr)
        }
    }

    impl<T> PartialEq for GcObj<T> {
        fn eq(&self, other: &Self) -> bool {
            self.ptr == other.ptr
        }
    }

    impl<T> Eq for GcObj<T> {}

    struct GcBox<T> {
        is_marked: Cell<bool>,
        next_obj: Cell<Option<Object>>,
        value: T,
    }
}
