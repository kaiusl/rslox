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

#[repr(C)]
pub union Value {
    raw: usize,
    number: f64,
    obj: Object,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.as_raw())
    }
}

impl Value {
    // In summary we use NaN values with extra data as a tag to indicate the type
    // of the value.
    // We use sign bit and 3 lowest bits in combination as tags.
    //
    // We assume that a pointer is actually 48bits in size and aligned to 8 byte
    // boundary, such that it's three lowest bits are always zero.
    //
    // # General safety comments:
    //
    // Number, bool and nil are directly stored inline.
    // All other values are stored as a pointer to some allocation.
    //
    // Self should only be created through a Self::new_.. method which takes a
    // valid object as an argument and takes ownership of it.
    //
    // Thus the underlying allocation is valid as long as Self is alive.
    //
    // It is always safe to access any of the union fields as they are all same
    // size and aligned to 8 byte boundary.
    // And none of them have any invalid bit patterns.
    //
    // Of course returning a specific value out needs to be checked before hand.
    // It can be done by either calling one of the self.is_.. methods or by comparing
    // the tag directly, for example self.get_tag() == Self::TAG_STRING.

    const QNAN: usize = 0x7ffc_0000_0000_0000;
    const SIGN_BIT: usize = 0x8000_0000_0000_0000;

    pub const TAG_NIL: usize = 0b001 | Self::QNAN;
    pub const TAG_FALSE: usize = 0b010 | Self::QNAN;
    pub const TAG_TRUE: usize = 0b011 | Self::QNAN;
    pub const TAG_STRING: usize = 0b100 | Self::QNAN;
    pub const TAG_FUNCTION: usize = 0b101 | Self::QNAN;
    pub const TAG_NATIVE_FN: usize = 0b110 | Self::QNAN;
    pub const TAG_CLOSURE: usize = 0b111 | Self::QNAN;
    pub const TAG_UPVALUE: usize = 0b010 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_CLASS: usize = 0b011 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_INSTANCE: usize = 0b100 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_BOUND_METHOD: usize = 0b110 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_BITS: usize = 0b111 | Self::QNAN | Self::SIGN_BIT;

    const NIL_VALUE: usize = Self::TAG_NIL;
    const FALSE_VALUE: usize = Self::TAG_FALSE;
    const TRUE_VALUE: usize = Self::TAG_TRUE;

    pub const TRUE: Self = Self {
        raw: Self::TRUE_VALUE,
    };
    pub const FALSE: Self = Self {
        raw: Self::FALSE_VALUE,
    };
    pub const NIL: Self = Self {
        raw: Self::NIL_VALUE,
    };

    pub fn new_number(n: f64) -> Self {
        Self { number: n }
    }

    pub fn new_bool(b: bool) -> Self {
        if b {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    pub fn new_object(obj: Object) -> Self {
        Self { obj }
    }

    pub fn new_string(obj: InternedString) -> Self {
        Self {
            obj: Object::new_string(obj),
        }
    }

    pub fn new_function(obj: GcObj<ObjFunction>) -> Self {
        Self {
            obj: Object::new_function(obj),
        }
    }

    pub fn new_native_fn(obj: GcObj<NativeFn>) -> Self {
        Self {
            obj: Object::new_native_fn(obj),
        }
    }

    pub fn new_closure(obj: GcObj<ObjClosure>) -> Self {
        Self {
            obj: Object::new_closure(obj),
        }
    }

    pub fn new_upvalue(obj: GcObj<RefCell<ObjUpvalue>>) -> Self {
        Self {
            obj: Object::new_upvalue(obj),
        }
    }

    pub fn new_class(obj: GcObj<RefCell<ObjClass>>) -> Self {
        Self {
            obj: Object::new_class(obj),
        }
    }

    pub fn new_instance(obj: GcObj<RefCell<ObjInstance>>) -> Self {
        Self {
            obj: Object::new_instance(obj),
        }
    }

    pub fn new_bound_method(obj: GcObj<ObjBoundMethod>) -> Self {
        Self {
            obj: Object::new_bound_method(obj),
        }
    }

    #[inline]
    fn as_raw(&self) -> usize {
        // SAFETY: this is always safe
        unsafe { self.raw }
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        (self.as_raw() & Self::QNAN) != Self::QNAN
    }

    #[inline]
    pub fn is_nil(&self) -> bool {
        self.as_raw() == Self::NIL_VALUE
    }

    #[inline]
    pub fn is_bool(&self) -> bool {
        // If self == FALSE_VALUE, | 1 sets the last bit, and makes it TRUE_VALUE
        // If self == TRUE_VALUE, | 1 makes is still TRUE_VALUE
        // in any other case it's some other value
        (self.as_raw() | 1) == Self::TRUE_VALUE
    }

    #[inline]
    pub fn try_as_number(&self) -> Option<f64> {
        if !self.is_number() {
            return None;
        }

        Some(unsafe { self.number })
    }

    #[inline]
    pub fn try_as_bool(&self) -> Option<bool> {
        if !self.is_bool() {
            return None;
        }

        Some(self.as_raw() == Self::TRUE_VALUE)
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        self.is_object_of_kind(Self::TAG_STRING)
    }

    #[inline]
    pub fn try_as_string(self) -> Option<InternedString> {
        if !self.is_string() {
            return None;
        }

        Some(unsafe { self.as_string_unchecked() })
    }

    /// # Safety
    ///
    /// Underlying type of self must be `InternedString`.
    #[inline]
    pub unsafe fn as_string_unchecked(self) -> InternedString {
        unsafe { self.obj.as_string_unchecked() }
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        self.is_object_of_kind(Self::TAG_FUNCTION)
    }

    #[inline]
    pub fn try_as_function(&self) -> Option<GcObj<ObjFunction>> {
        if !self.is_function() {
            return None;
        }

        unsafe { Some(self.as_function_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `ObjFunction`.
    #[inline]
    pub unsafe fn as_function_unchecked(self) -> GcObj<ObjFunction> {
        unsafe { self.obj.as_function_unchecked() }
    }

    #[inline]
    pub fn is_native_fn(&self) -> bool {
        self.is_object_of_kind(Self::TAG_NATIVE_FN)
    }

    #[inline]
    pub fn try_as_native_fn(self) -> Option<GcObj<NativeFn>> {
        if !self.is_native_fn() {
            return None;
        }

        unsafe { Some(self.as_native_fn_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `NativeFn`.
    #[inline]
    pub unsafe fn as_native_fn_unchecked(self) -> GcObj<NativeFn> {
        unsafe { self.obj.as_native_fn_unchecked() }
    }

    #[inline]
    pub fn is_closure(&self) -> bool {
        self.is_object_of_kind(Self::TAG_CLOSURE)
    }

    #[inline]
    pub fn try_as_closure(self) -> Option<GcObj<ObjClosure>> {
        if self.is_closure() {
            unsafe { Some(self.as_closure_unchecked()) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `ObjClosure`.
    #[inline]
    pub unsafe fn as_closure_unchecked(self) -> GcObj<ObjClosure> {
        unsafe { self.obj.as_closure_unchecked() }
    }

    #[inline]
    pub fn is_upvalue(&self) -> bool {
        self.is_object_of_kind(Self::TAG_UPVALUE)
    }

    #[inline]
    pub fn try_as_upvalue(&self) -> Option<GcObj<RefCell<ObjUpvalue>>> {
        if !self.is_upvalue() {
            return None;
        }

        unsafe { Some(self.as_upvalue_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `RefCell<ObjUpvalue>`.
    #[inline]
    pub unsafe fn as_upvalue_unchecked(self) -> GcObj<RefCell<ObjUpvalue>> {
        unsafe { self.obj.as_upvalue_unchecked() }
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        self.is_object_of_kind(Self::TAG_CLASS)
    }

    #[inline]
    pub fn try_as_class(self) -> Option<GcObj<RefCell<ObjClass>>> {
        if !self.is_class() {
            return None;
        }

        unsafe { Some(self.as_class_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be RefCell<ObjClass>.
    #[inline]
    pub unsafe fn as_class_unchecked(self) -> GcObj<RefCell<ObjClass>> {
        unsafe { self.obj.as_class_unchecked() }
    }

    #[inline]
    pub fn is_instance(&self) -> bool {
        self.is_object_of_kind(Self::TAG_INSTANCE)
    }

    #[inline]
    pub fn try_as_instance(&self) -> Option<GcObj<RefCell<ObjInstance>>> {
        if self.is_instance() {
            unsafe { Some(self.as_instance_unchecked()) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `RefCell<ObjInstance>`.
    #[inline]
    pub unsafe fn as_instance_unchecked(self) -> GcObj<RefCell<ObjInstance>> {
        unsafe { self.obj.as_instance_unchecked() }
    }

    #[inline]
    pub fn is_bound_method(&self) -> bool {
        self.is_object_of_kind(Self::TAG_BOUND_METHOD)
    }

    #[inline]
    pub fn try_as_bound_method(self) -> Option<GcObj<ObjBoundMethod>> {
        if !self.is_bound_method() {
            return None;
        }

        unsafe { Some(self.as_bound_method_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be ObjBoundMethod.
    #[inline]
    pub unsafe fn as_bound_method_unchecked(self) -> GcObj<ObjBoundMethod> {
        unsafe { self.obj.as_bound_method_unchecked() }
    }

    #[inline]
    pub fn is_object(&self) -> bool {
        !self.is_number() && !self.is_bool() && !self.is_nil()
    }

    #[inline]
    fn is_object_of_kind(&self, tag: usize) -> bool {
        self.as_raw() & Self::TAG_BITS == tag
    }

    #[inline]
    pub fn get_tag(&self) -> usize {
        self.as_raw() & Self::TAG_BITS
    }

    #[inline]
    pub fn is_falsey(&self) -> bool {
        self.as_raw() == Self::NIL_VALUE || self.as_raw() == Self::FALSE_VALUE
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        // check numbers specifically so that we get NaN != NaN
        if let (Some(s), Some(o)) = (self.try_as_number(), other.try_as_number()) {
            return s == o;
        }

        self.as_raw() == other.as_raw()
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for Value {}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_number() {
            return write!(f, "{}", self.try_as_number().unwrap());
        }

        match self.get_tag() {
            Self::TAG_FALSE => write!(f, "false"),
            Self::TAG_TRUE => write!(f, "true"),
            Self::TAG_NIL => write!(f, "nil"),
            Self::TAG_STRING => {
                let obj = self.try_as_string().unwrap();
                write!(f, "{}", *obj)
            }
            Self::TAG_FUNCTION => {
                let obj = self.try_as_function().unwrap();
                write!(f, "{}", *obj)
            }
            Self::TAG_NATIVE_FN => write!(f, "<native fun>"),
            Self::TAG_CLOSURE => {
                let obj = self.try_as_closure().unwrap();
                write!(f, "{}", *obj)
            }
            Self::TAG_UPVALUE => {
                let obj = self.try_as_upvalue().unwrap();
                let obj = RefCell::borrow(&*obj);
                write!(f, "{}", obj)
            }
            Self::TAG_CLASS => {
                let obj = self.try_as_class().unwrap();
                let obj = RefCell::borrow(&*obj);
                write!(f, "{}", obj)
            }
            Self::TAG_INSTANCE => {
                let obj = self.try_as_instance().unwrap();
                let obj = RefCell::borrow(&*obj);
                write!(f, "{}", obj)
            }
            Self::TAG_BOUND_METHOD => {
                let obj = self.try_as_bound_method().unwrap();
                write!(f, "{}", *obj)
            }

            _ => unreachable!(),
        }
    }
}

#[repr(C)]
pub union Object {
    raw: usize,
    // Used to access common fields of GcObj, where it doesn't actually matter what type is stored there
    // The internal GcBox must be repr(C) for this to work.
    common_obj: gc::TaggedGcObjCommon,
    string: TaggedGcObj<InternedStringCore, { Value::TAG_STRING }>,
    function: TaggedGcObj<ObjFunction, { Value::TAG_FUNCTION }>,
    native_fn: TaggedGcObj<NativeFn, { Value::TAG_NATIVE_FN }>,
    closure: TaggedGcObj<ObjClosure, { Value::TAG_CLOSURE }>,
    upvalue: TaggedGcObj<RefCell<ObjUpvalue>, { Value::TAG_UPVALUE }>,
    class: TaggedGcObj<RefCell<ObjClass>, { Value::TAG_CLASS }>,
    instance: TaggedGcObj<RefCell<ObjInstance>, { Value::TAG_INSTANCE }>,
    bound_method: TaggedGcObj<ObjBoundMethod, { Value::TAG_BOUND_METHOD }>,
}

impl Copy for Object {}

impl Clone for Object {
    fn clone(&self) -> Self {
        *self
    }
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.as_raw())
    }
}

impl Object {
    pub fn new_string(obj: InternedString) -> Self {
        Self {
            string: TaggedGcObj::new(obj.0),
        }
    }

    pub fn new_function(obj: GcObj<ObjFunction>) -> Self {
        Self {
            function: TaggedGcObj::new(obj),
        }
    }

    pub fn new_native_fn(obj: GcObj<NativeFn>) -> Self {
        Self {
            native_fn: TaggedGcObj::new(obj),
        }
    }

    pub fn new_closure(obj: GcObj<ObjClosure>) -> Self {
        Self {
            closure: TaggedGcObj::new(obj),
        }
    }

    pub fn new_upvalue(obj: GcObj<RefCell<ObjUpvalue>>) -> Self {
        Self {
            upvalue: TaggedGcObj::new(obj),
        }
    }

    pub fn new_class(obj: GcObj<RefCell<ObjClass>>) -> Self {
        Self {
            class: TaggedGcObj::new(obj),
        }
    }

    pub fn new_instance(obj: GcObj<RefCell<ObjInstance>>) -> Self {
        Self {
            instance: TaggedGcObj::new(obj),
        }
    }

    pub fn new_bound_method(obj: GcObj<ObjBoundMethod>) -> Self {
        Self {
            bound_method: TaggedGcObj::new(obj),
        }
    }

    #[inline]
    fn as_raw(&self) -> usize {
        // SAFETY: this is always safe
        unsafe { self.raw }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        self.is_object_of_kind(Value::TAG_STRING)
    }

    #[inline]
    pub fn try_as_string(self) -> Option<InternedString> {
        if !self.is_string() {
            return None;
        }
        Some(unsafe { self.as_string_unchecked() })
    }

    /// # Safety
    ///
    /// Underlying type of self must be `InternedString`.
    #[inline]
    pub unsafe fn as_string_unchecked(self) -> InternedString {
        let inner = unsafe { self.string.untag() };
        InternedString(inner)
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        self.is_object_of_kind(Value::TAG_FUNCTION)
    }

    #[inline]
    pub fn try_as_function(self) -> Option<GcObj<ObjFunction>> {
        if !self.is_function() {
            return None;
        }

        unsafe { Some(self.as_function_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `ObjFunction`.
    #[inline]
    pub unsafe fn as_function_unchecked(self) -> GcObj<ObjFunction> {
        unsafe { self.function.untag() }
    }

    #[inline]
    pub fn is_native_fn(&self) -> bool {
        self.is_object_of_kind(Value::TAG_NATIVE_FN)
    }

    #[inline]
    pub fn try_as_native_fn(self) -> Option<GcObj<NativeFn>> {
        if !self.is_native_fn() {
            return None;
        }

        unsafe { Some(self.as_native_fn_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `NativeFn`.
    #[inline]
    pub unsafe fn as_native_fn_unchecked(self) -> GcObj<NativeFn> {
        unsafe { self.native_fn.untag() }
    }

    #[inline]
    pub fn is_closure(&self) -> bool {
        self.is_object_of_kind(Value::TAG_CLOSURE)
    }

    #[inline]
    pub fn try_as_closure(self) -> Option<GcObj<ObjClosure>> {
        if self.is_closure() {
            unsafe { Some(self.as_closure_unchecked()) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `ObjClosure`.
    #[inline]
    pub unsafe fn as_closure_unchecked(self) -> GcObj<ObjClosure> {
        unsafe { self.closure.untag() }
    }

    #[inline]
    pub fn is_upvalue(&self) -> bool {
        self.is_object_of_kind(Value::TAG_UPVALUE)
    }

    #[inline]
    pub fn try_as_upvalue(&self) -> Option<GcObj<RefCell<ObjUpvalue>>> {
        if !self.is_upvalue() {
            return None;
        }

        unsafe { Some(self.as_upvalue_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `RefCell<ObjUpvalue>`.
    #[inline]
    pub unsafe fn as_upvalue_unchecked(self) -> GcObj<RefCell<ObjUpvalue>> {
        unsafe { self.upvalue.untag() }
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        self.is_object_of_kind(Value::TAG_CLASS)
    }

    #[inline]
    pub fn try_as_class(self) -> Option<GcObj<RefCell<ObjClass>>> {
        if !self.is_class() {
            return None;
        }

        unsafe { Some(self.as_class_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be RefCell<ObjClass>.
    #[inline]
    pub unsafe fn as_class_unchecked(self) -> GcObj<RefCell<ObjClass>> {
        unsafe { self.class.untag() }
    }

    #[inline]
    pub fn is_instance(&self) -> bool {
        self.is_object_of_kind(Value::TAG_INSTANCE)
    }

    #[inline]
    pub fn try_as_instance(&self) -> Option<GcObj<RefCell<ObjInstance>>> {
        if self.is_instance() {
            unsafe { Some(self.as_instance_unchecked()) }
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be RefCell<ObjInstance>.
    #[inline]
    pub unsafe fn as_instance_unchecked(self) -> GcObj<RefCell<ObjInstance>> {
        unsafe { self.instance.untag() }
    }

    #[inline]
    pub fn is_bound_method(&self) -> bool {
        self.is_object_of_kind(Value::TAG_BOUND_METHOD)
    }

    #[inline]
    pub fn try_as_bound_method(self) -> Option<GcObj<ObjBoundMethod>> {
        if !self.is_bound_method() {
            return None;
        }

        unsafe { Some(self.as_bound_method_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be ObjBoundMethod.
    #[inline]
    pub unsafe fn as_bound_method_unchecked(self) -> GcObj<ObjBoundMethod> {
        unsafe { self.bound_method.untag() }
    }

    #[inline]
    fn is_object_of_kind(&self, tag: usize) -> bool {
        self.as_raw() & Value::TAG_BITS == tag
    }

    #[inline]
    pub fn get_tag(&self) -> usize {
        self.as_raw() & Value::TAG_BITS
    }

    #[inline]
    pub fn is_falsey(&self) -> bool {
        self.as_raw() == Value::NIL_VALUE || self.as_raw() == Value::FALSE_VALUE
    }
}

impl From<GcObj<InternedStringCore>> for Object {
    fn from(value: GcObj<InternedStringCore>) -> Self {
        Object::new_string(InternedString(value))
    }
}

impl From<GcObj<ObjFunction>> for Object {
    fn from(fun: GcObj<ObjFunction>) -> Self {
        Object::new_function(fun)
    }
}

impl From<GcObj<NativeFn>> for Object {
    fn from(fun: GcObj<NativeFn>) -> Self {
        Object::new_native_fn(fun)
    }
}

impl From<GcObj<ObjClosure>> for Object {
    fn from(fun: GcObj<ObjClosure>) -> Self {
        Object::new_closure(fun)
    }
}

impl From<GcObj<RefCell<ObjUpvalue>>> for Object {
    fn from(upvalue: GcObj<RefCell<ObjUpvalue>>) -> Self {
        Object::new_upvalue(upvalue)
    }
}

impl From<GcObj<RefCell<ObjClass>>> for Object {
    fn from(cls: GcObj<RefCell<ObjClass>>) -> Self {
        Object::new_class(cls)
    }
}

impl From<GcObj<RefCell<ObjInstance>>> for Object {
    fn from(inst: GcObj<RefCell<ObjInstance>>) -> Self {
        Object::new_instance(inst)
    }
}

impl From<GcObj<ObjBoundMethod>> for Object {
    fn from(bm: GcObj<ObjBoundMethod>) -> Self {
        Object::new_bound_method(bm)
    }
}

impl Object {
    fn alloc_addr(&self) -> usize {
        unsafe {
            match self.get_tag() {
                Value::TAG_STRING => self.as_string_unchecked().0.alloc_addr(),
                Value::TAG_FUNCTION => self.as_function_unchecked().alloc_addr(),
                Value::TAG_NATIVE_FN => self.as_native_fn_unchecked().alloc_addr(),
                Value::TAG_CLOSURE => self.as_closure_unchecked().alloc_addr(),
                Value::TAG_UPVALUE => self.as_upvalue_unchecked().alloc_addr(),
                Value::TAG_CLASS => self.as_class_unchecked().alloc_addr(),
                Value::TAG_INSTANCE => self.as_instance_unchecked().alloc_addr(),
                _ => unreachable!(),
            }
        }
    }
}

impl fmt::Pointer for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#x}", self.alloc_addr())
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            match self.get_tag() {
                Value::TAG_STRING => {
                    let obj = self.as_string_unchecked();
                    write!(f, "{}", *obj)
                }
                Value::TAG_FUNCTION => {
                    let obj = self.as_function_unchecked();
                    write!(f, "{}", *obj)
                }
                Value::TAG_NATIVE_FN => write!(f, "<native fun>"),
                Value::TAG_CLOSURE => {
                    let obj = self.as_closure_unchecked();
                    write!(f, "{}", *obj)
                }
                Value::TAG_UPVALUE => {
                    let obj = self.as_upvalue_unchecked();
                    let obj = RefCell::borrow(&*obj);
                    write!(f, "{}", obj)
                }
                Value::TAG_CLASS => {
                    let obj = self.as_class_unchecked();
                    let obj = RefCell::borrow(&*obj);
                    write!(f, "{}", obj)
                }
                Value::TAG_INSTANCE => {
                    let obj = self.as_instance_unchecked();
                    let obj = RefCell::borrow(&*obj);
                    write!(f, "{}", obj)
                }
                Value::TAG_BOUND_METHOD => {
                    let obj = self.as_bound_method_unchecked();
                    write!(f, "{}", *obj)
                }
                _ => unreachable!(),
            }
        }
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.as_raw() == other.as_raw()
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

use self::gc::TaggedGcObj;

mod gc {
    use std::num::NonZero;

    use super::*;

    /// Helper functions on Object that are only relevant to the garbage collector
    impl Object {
        fn set_mark(&self) {
            #[cfg(feature = "debug_gc")]
            {
                println!("gc: marked [{:#x}] {}", self.alloc_addr(), self);
            }
            unsafe {
                self.common_obj.untag().set_mark();
            }
        }

        fn is_marked(&self) -> bool {
            unsafe { self.common_obj.untag().is_marked() }
        }

        fn clean_mark(&self) {
            #[cfg(feature = "debug_gc")]
            {
                println!("gc: cleaned mark [{:#x}] {}", self.alloc_addr(), self);
            }
            unsafe {
                self.common_obj.untag().clean_mark();
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
                match self.get_tag() {
                    Value::TAG_STRING => self.as_string_unchecked().0.free(),
                    Value::TAG_FUNCTION => self.as_function_unchecked().free(),
                    Value::TAG_NATIVE_FN => self.as_native_fn_unchecked().free(),
                    Value::TAG_CLOSURE => self.as_closure_unchecked().free(),
                    Value::TAG_UPVALUE => self.as_upvalue_unchecked().free(),
                    Value::TAG_CLASS => self.as_class_unchecked().free(),
                    Value::TAG_INSTANCE => self.as_instance_unchecked().free(),
                    Value::TAG_BOUND_METHOD => self.as_bound_method_unchecked().free(),
                    _ => unreachable!(),
                }
            }
        }

        fn alloc_size(&self) -> usize {
            unsafe {
                match self.get_tag() {
                    Value::TAG_STRING => self.as_string_unchecked().0.alloc_size(),
                    Value::TAG_FUNCTION => self.as_function_unchecked().alloc_size(),
                    Value::TAG_NATIVE_FN => self.as_native_fn_unchecked().alloc_size(),
                    Value::TAG_CLOSURE => self.as_closure_unchecked().alloc_size(),
                    Value::TAG_UPVALUE => self.as_upvalue_unchecked().alloc_size(),
                    Value::TAG_CLASS => self.as_class_unchecked().alloc_size(),
                    Value::TAG_INSTANCE => self.as_instance_unchecked().alloc_size(),
                    Value::TAG_BOUND_METHOD => self.as_bound_method_unchecked().alloc_size(),
                    _ => unreachable!(),
                }
            }
        }

        /// # Safety
        ///
        /// Caller must ensure that `obj` is not already in the linked list of objects, eg cycles are not allowed.
        /// Otherwise gc may access already freed memory during the sweep or get stuck in infinite loop.
        unsafe fn set_next_obj(&self, obj: Option<Object>) {
            unsafe { self.common_obj.untag().set_next_obj(obj) }
        }

        fn next_obj(&self) -> Option<Object> {
            unsafe { self.common_obj.untag().next_obj() }
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

        pub fn new_value<T>(&mut self, value: T) -> Value
        where
            GcObj<T>: Into<Object>,
        {
            Value::new_object(self.new_object(value))
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
            let closure = Object::new_closure(frame.closure);
            self.mark_obj(closure);
        }

        pub fn mark_value(&mut self, v: &Value) {
            if v.is_object() {
                self.mark_obj(unsafe { v.obj });
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
            println!("gc: tracing references");

            while let Some(obj) = self.gray_stack.pop() {
                self.blacken_obj(obj);
            }

            #[cfg(feature = "debug_gc")]
            println!("gc: tracing references -- done");
        }

        fn blacken_obj(&mut self, obj: Object) {
            #[cfg(feature = "debug_gc")]
            println!("gc: blackened [{:p}] {}", obj, obj);

            match obj.get_tag() {
                Value::TAG_STRING | Value::TAG_NATIVE_FN => {}
                Value::TAG_FUNCTION => {
                    let fun = unsafe { obj.function.untag() };

                    let name = Object::new_string(fun.name);
                    self.mark_obj(name);
                    for it in fun.constants.iter() {
                        self.mark_value(it);
                    }
                }
                Value::TAG_CLOSURE => {
                    let c = unsafe { obj.closure.untag() };
                    let fun = Object::new_function(c.fun);
                    self.mark_obj(fun);
                    for it in c.upvalues.iter() {
                        let it = Object::new_upvalue(*it);
                        self.mark_obj(it);
                    }
                }
                Value::TAG_UPVALUE => {
                    let o = unsafe { obj.upvalue.untag() };

                    let o = o.borrow();
                    if let ObjUpvalue::Closed(obj) = o.deref() {
                        self.mark_value(obj);
                    }
                }
                Value::TAG_CLASS => {
                    let c = unsafe { obj.class.untag() };
                    let c = c.borrow();
                    let name = Object::new_string(c.name);
                    self.mark_obj(name);
                    for it in c.methods.iter() {
                        it.0 .0.set_mark();
                        self.mark_value(it.1);
                    }
                }
                Value::TAG_INSTANCE => {
                    let i = unsafe { obj.instance.untag() };
                    let i = i.borrow();
                    let class = Object::new_class(i.class);
                    self.mark_obj(class);
                    for it in i.properties.iter() {
                        it.0 .0.set_mark();
                        self.mark_value(it.1);
                    }
                }
                Value::TAG_BOUND_METHOD => {
                    let bm = unsafe { obj.bound_method.untag() };
                    self.mark_value(&bm.receiver);
                    let method = Object::new_closure(bm.method);
                    self.mark_obj(method);
                }
                _ => unreachable!(),
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

    #[repr(transparent)]
    pub struct TaggedGcObj<T, const N: usize> {
        ptr: ptr::NonNull<GcBox<T>>,
        marker: PhantomData<GcBox<T>>,
    }

    #[repr(transparent)]
    pub(super) struct TaggedGcObjCommon {
        // IMPORTANT: This struct Must have the same layout as TaggedGcObj<T> as they will be cast between in Object union
        ptr: ptr::NonNull<GcBoxCommon>,
        marker: PhantomData<GcBoxCommon>,
    }

    impl<T, const TAG: usize> TaggedGcObj<T, TAG> {
        pub fn new(data: GcObj<T>) -> Self {
            Self {
                ptr: data.ptr.map_addr(|a| a | TAG),
                marker: PhantomData,
            }
        }

        pub fn untag(self) -> GcObj<T> {
            GcObj {
                ptr: self
                    .ptr
                    .map_addr(|a| unsafe { NonZero::new_unchecked(a.get() & !TAG) }),
                marker: PhantomData,
            }
        }
    }

    impl<T, const N: usize> Copy for TaggedGcObj<T, N> {}

    impl<T, const N: usize> Clone for TaggedGcObj<T, N> {
        fn clone(&self) -> Self {
            *self
        }
    }

    impl TaggedGcObjCommon {
        pub(super) fn untag(self) -> GcObjCommon {
            GcObjCommon {
                ptr: self
                    .ptr
                    .map_addr(|a| unsafe { NonZero::new_unchecked(a.get() & !Value::TAG_BITS) }),
            }
        }
    }

    impl Copy for TaggedGcObjCommon {}

    impl Clone for TaggedGcObjCommon {
        fn clone(&self) -> Self {
            *self
        }
    }

    #[repr(transparent)]
    pub struct GcObj<T> {
        ptr: ptr::NonNull<GcBox<T>>,
        marker: PhantomData<GcBox<T>>,
    }

    #[repr(transparent)]
    pub(super) struct GcObjCommon {
        ptr: ptr::NonNull<GcBoxCommon>,
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
            let inner = GcBox::new(data);
            let ptr = Box::into_raw(Box::new(inner));

            #[cfg(feature = "debug_gc")]
            println!("gc: created [{:p}]", ptr);
            GcObj {
                ptr: unsafe { ptr::NonNull::new_unchecked(ptr) },
                marker: PhantomData,
            }
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
            self.as_inner().common.is_marked.set(true);
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

    impl GcObjCommon {
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
        fn as_inner(&self) -> &GcBoxCommon {
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
    }

    // IMPORTANT: a pointer to GcBox<T> must also be a valid pointer to GcBoxCommon,
    //    that is both must be repr(C) and common must be first field on GcBox<T>
    //
    // This is needed to access common fields of GcObj, where it doesn't actually matter what type is stored there.
    #[repr(C)]
    struct GcBox<T> {
        common: GcBoxCommon,
        value: T,
    }

    #[repr(C)]
    struct GcBoxCommon {
        is_marked: Cell<bool>,
        next_obj: Cell<Option<Object>>,
    }

    impl<T> GcBox<T> {
        fn new(value: T) -> Self {
            Self {
                common: GcBoxCommon {
                    is_marked: Cell::new(false),
                    next_obj: Cell::new(None),
                },
                value,
            }
        }
    }

    #[test]
    fn gc_box() {
        let b = unsafe { GcObj::<ObjClass>::dangling() };

        // common is the first field on GcBox
        assert_eq!(
            unsafe { ptr::addr_of!((*b.ptr.as_ptr()).common) },
            b.ptr.as_ptr() as *const GcBoxCommon
        );
    }
}
