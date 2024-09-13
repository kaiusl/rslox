use core::fmt;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::hash::BuildHasher as _;
use std::marker::PhantomData;
use std::mem::{self, ManuallyDrop};
use std::ops::{self, Deref};
use std::rc::{Rc, Weak};
use std::sync::Arc;

use hashbrown::HashMap;

use crate::bytecode::ByteCode;
use crate::common::Span;
use crate::vm::BuildHasher;

#[repr(C)]
pub union Value {
    raw: usize,
    number: f64,
    string: *const InternedStringCore,
    function: *const ObjFunction,
    native_fn: *const NativeFn,
    closure: *const ObjClosure,
    upvalue: *const RefCell<ObjUpvalue>,
    class: *const RefCell<ObjClass>,
    instance: *const RefCell<ObjInstance>,
    bound_method: *const ObjBoundMethod,
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
    // valid Rc/Arc as an argument and takes ownership of it.
    // That value is dropped once Self is dropped or converted back into inner Rc/Arc.
    //
    // Thus the underlying allocation is valid as long as Self is alive.
    //
    // Since all the objects are Rc/Arc based then care needs to be taken to
    // ensure that strong and weak counts are properly increased
    // when either cloning, dropping or converting into Rc/Arc.
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
    pub const TAG_WEAK_CLOSURE: usize = 0b001 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_UPVALUE: usize = 0b010 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_CLASS: usize = 0b011 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_INSTANCE: usize = 0b100 | Self::QNAN | Self::SIGN_BIT;
    pub const TAG_WEAK_INSTANCE: usize = 0b101 | Self::QNAN | Self::SIGN_BIT;
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

    pub fn new_string(obj: InternedString) -> Self {
        Self {
            string: Arc::into_raw(obj.0).map_addr(|a| a | Self::TAG_STRING),
        }
    }

    pub fn new_function(obj: Rc<ObjFunction>) -> Self {
        Self {
            function: Rc::into_raw(obj).map_addr(|a| a | Self::TAG_FUNCTION),
        }
    }

    pub fn new_native_fn(obj: Rc<NativeFn>) -> Self {
        Self {
            native_fn: Rc::into_raw(obj).map_addr(|a| a | Self::TAG_NATIVE_FN),
        }
    }

    pub fn new_closure(obj: Rc<ObjClosure>) -> Self {
        Self {
            closure: Rc::into_raw(obj).map_addr(|a| a | Self::TAG_CLOSURE),
        }
    }

    pub fn new_weak_closure(obj: Weak<ObjClosure>) -> Self {
        Self {
            closure: Weak::into_raw(obj).map_addr(|a| a | Self::TAG_WEAK_CLOSURE),
        }
    }

    pub fn new_upvalue(obj: Rc<RefCell<ObjUpvalue>>) -> Self {
        Self {
            upvalue: Rc::into_raw(obj).map_addr(|a| a | Self::TAG_UPVALUE),
        }
    }

    pub fn new_class(obj: Rc<RefCell<ObjClass>>) -> Self {
        Self {
            class: Rc::into_raw(obj).map_addr(|a| a | Self::TAG_CLASS),
        }
    }

    pub fn new_instance(obj: Rc<RefCell<ObjInstance>>) -> Self {
        Self {
            instance: Rc::into_raw(obj).map_addr(|a| a | Self::TAG_INSTANCE),
        }
    }

    pub fn new_weak_instance(obj: Weak<RefCell<ObjInstance>>) -> Self {
        Self {
            instance: Weak::into_raw(obj).map_addr(|a| a | Self::TAG_WEAK_INSTANCE),
        }
    }

    pub fn new_bound_method(obj: Rc<ObjBoundMethod>) -> Self {
        Self {
            bound_method: Rc::into_raw(obj).map_addr(|addr| addr | Self::TAG_BOUND_METHOD),
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
    pub fn try_to_number(&self) -> Option<f64> {
        if !self.is_number() {
            return None;
        }

        Some(unsafe { self.number })
    }

    #[inline]
    pub fn try_to_bool(&self) -> Option<bool> {
        if !self.is_bool() {
            return None;
        }

        Some(self.as_raw() == Self::TRUE_VALUE)
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        self.is_object(Self::TAG_STRING)
    }

    #[inline]
    pub fn try_as_string(&self) -> Option<ArcRef<'_, InternedStringCore>> {
        if !self.is_string() {
            return None;
        }

        Some(unsafe { self.as_string_unchecked() })
    }

    /// # Safety
    ///
    /// Underlying type of self must be `InternedString`.
    #[inline]
    pub unsafe fn as_string_unchecked(&self) -> ArcRef<'_, InternedStringCore> {
        unsafe { self.as_arc_unchecked(self.string) }
    }

    #[inline]
    pub fn try_to_string(&self) -> Option<InternedString> {
        if !self.is_string() {
            return None;
        }
        Some(unsafe { self.to_string_unchecked() })
    }

    /// # Safety
    ///
    /// Underlying type of self must be `InternedString`.
    #[inline]
    pub unsafe fn to_string_unchecked(&self) -> InternedString {
        let inner = unsafe { self.to_arc_unchecked(self.string) };
        InternedString(inner)
    }

    #[inline]
    pub fn try_into_string(self) -> Option<InternedString> {
        if !self.is_string() {
            return None;
        }
        Some(unsafe { self.into_string_unchecked() })
    }

    /// # Safety
    ///
    /// Underlying type of self must be `InternedString`.
    #[inline]
    pub unsafe fn into_string_unchecked(self) -> InternedString {
        let inner = unsafe {
            let ptr = self.string;
            self.into_arc_unchecked(ptr)
        };
        InternedString(inner)
    }

    #[inline]
    pub fn is_function(&self) -> bool {
        self.is_object(Self::TAG_FUNCTION)
    }

    #[inline]
    pub fn try_as_function(&self) -> Option<RcRef<'_, ObjFunction>> {
        if !self.is_function() {
            return None;
        }

        unsafe { Some(self.as_rc_unchecked(self.function)) }
    }

    #[inline]
    pub fn is_native_fn(&self) -> bool {
        self.is_object(Self::TAG_NATIVE_FN)
    }

    #[inline]
    pub fn try_as_native_fn(&self) -> Option<RcRef<'_, NativeFn>> {
        if !self.is_native_fn() {
            return None;
        }

        unsafe { Some(self.as_rc_unchecked(self.native_fn)) }
    }

    #[inline]
    pub fn try_into_native_fn(self) -> Option<Rc<NativeFn>> {
        if !self.is_native_fn() {
            return None;
        }

        unsafe { Some(self.into_native_fn_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `Rc<NativeFn>`.
    #[inline]
    pub unsafe fn into_native_fn_unchecked(self) -> Rc<NativeFn> {
        unsafe {
            let ptr = self.native_fn;
            self.into_rc_unchecked(ptr)
        }
    }

    #[inline]
    pub fn is_closure(&self) -> bool {
        self.is_object(Self::TAG_CLOSURE)
    }

    #[inline]
    pub fn try_as_closure(&self) -> Option<CowRcRef<'_, ObjClosure>> {
        if self.is_closure() {
            let rc_ref = unsafe { self.as_rc_unchecked(self.closure) };
            return Some(CowRcRef::Borrowed(rc_ref));
        } else if self.is_weak_closure() {
            let obj = unsafe { self.weak_to_rc_unchecked(self.closure) };
            return Some(CowRcRef::Owned(obj));
        } else {
            None
        }
    }

    #[inline]
    pub fn try_to_closure(&self) -> Option<Rc<ObjClosure>> {
        if self.is_closure() {
            Some(unsafe { self.to_rc_unchecked(self.closure) })
        } else if self.is_weak_closure() {
            Some(unsafe { self.weak_to_rc_unchecked(self.closure) })
        } else {
            None
        }
    }

    #[inline]
    pub fn try_into_closure(self) -> Option<Rc<ObjClosure>> {
        if self.is_closure() {
            unsafe { Some(self.into_closure_unchecked()) }
        } else if self.is_weak_closure() {
            let weak = unsafe { self.into_weak_closure_unchecked() };
            weak.upgrade()
        } else {
            None
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `Rc<ObjClosure>`.
    #[inline]
    pub unsafe fn into_closure_unchecked(self) -> Rc<ObjClosure> {
        unsafe {
            let ptr = self.closure;
            self.into_rc_unchecked(ptr)
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be `rc::Weak<ObjClosure`.
    #[inline]
    pub unsafe fn into_weak_closure_unchecked(self) -> Weak<ObjClosure> {
        unsafe {
            let ptr = self.closure;
            self.into_rc_weak_unchecked(ptr)
        }
    }

    #[inline]
    pub fn is_weak_closure(&self) -> bool {
        self.is_object(Self::TAG_WEAK_CLOSURE)
    }

    #[inline]
    pub fn is_upvalue(&self) -> bool {
        self.is_object(Self::TAG_UPVALUE)
    }

    #[inline]
    pub fn try_as_upvalue(&self) -> Option<RcRef<'_, RefCell<ObjUpvalue>>> {
        if !self.is_upvalue() {
            return None;
        }

        unsafe { Some(self.as_rc_unchecked(self.upvalue)) }
    }

    #[inline]
    pub fn is_class(&self) -> bool {
        self.is_object(Self::TAG_CLASS)
    }

    #[inline]
    pub fn try_as_class(&self) -> Option<RcRef<'_, RefCell<ObjClass>>> {
        if !self.is_class() {
            return None;
        }

        unsafe { Some(self.as_rc_unchecked(self.class)) }
    }

    #[inline]
    pub fn try_into_class(self) -> Option<Rc<RefCell<ObjClass>>> {
        if !self.is_class() {
            return None;
        }

        unsafe { Some(self.into_class_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be Rc<RefCell<ObjClass>>.
    #[inline]
    pub unsafe fn into_class_unchecked(self) -> Rc<RefCell<ObjClass>> {
        unsafe {
            let ptr = self.class;
            self.into_rc_unchecked(ptr)
        }
    }

    #[inline]
    pub fn is_instance(&self) -> bool {
        self.is_object(Self::TAG_INSTANCE)
    }

    #[inline]
    pub fn try_as_instance(&self) -> Option<CowRcRef<'_, RefCell<ObjInstance>>> {
        if self.is_instance() {
            let rc_ref = unsafe { self.as_rc_unchecked(self.instance) };
            return Some(CowRcRef::Borrowed(rc_ref));
        } else if self.is_weak_instance() {
            let obj = unsafe { self.weak_to_rc_unchecked(self.instance) };
            Some(CowRcRef::Owned(obj))
        } else {
            None
        }
    }

    #[inline]
    pub fn is_weak_instance(&self) -> bool {
        self.is_object(Self::TAG_WEAK_INSTANCE)
    }

    #[inline]
    pub fn is_bound_method(&self) -> bool {
        self.is_object(Self::TAG_BOUND_METHOD)
    }

    #[inline]
    pub fn try_as_bound_method(&self) -> Option<RcRef<'_, ObjBoundMethod>> {
        if !self.is_bound_method() {
            return None;
        }

        unsafe { Some(self.as_rc_unchecked(self.bound_method)) }
    }

    #[inline]
    pub fn try_into_bound_method(self) -> Option<Rc<ObjBoundMethod>> {
        if !self.is_bound_method() {
            return None;
        }

        unsafe { Some(self.into_bound_method_unchecked()) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be Rc<ObjBoundMethod>.
    #[inline]
    pub unsafe fn into_bound_method_unchecked(self) -> Rc<ObjBoundMethod> {
        unsafe {
            let ptr = self.bound_method;
            self.into_rc_unchecked(ptr)
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be Rc<T>.
    #[inline]
    unsafe fn as_rc_unchecked<T>(&self, ptr: *const T) -> RcRef<'_, T> {
        unsafe {
            let ptr = ptr.map_addr(Self::to_object_addr);
            RcRef::new_unchecked(ptr)
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be Arc<T>.
    #[inline]
    unsafe fn as_arc_unchecked<T>(&self, ptr: *const T) -> ArcRef<'_, T> {
        unsafe {
            let ptr = ptr.map_addr(Self::to_object_addr);
            ArcRef::new_unchecked(ptr)
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be Arc<T>.
    #[inline]
    unsafe fn to_arc_unchecked<T>(&self, ptr: *const T) -> Arc<T> {
        unsafe {
            let ptr = ptr.map_addr(Self::to_object_addr);
            Arc::increment_strong_count(ptr);
            Arc::from_raw(ptr)
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be Rc<T>.
    #[inline]
    unsafe fn to_rc_unchecked<T>(&self, ptr: *const T) -> Rc<T> {
        unsafe {
            let ptr = ptr.map_addr(Self::to_object_addr);
            Rc::increment_strong_count(ptr);
            Rc::from_raw(ptr)
        }
    }

    /// # Safety
    ///
    /// Underlying type of self must be rc::Weak<T>.
    #[inline]
    unsafe fn weak_to_rc_unchecked<T>(&self, ptr: *const T) -> Rc<T> {
        let ptr = ptr.map_addr(Self::to_object_addr);
        let weak = unsafe { Weak::from_raw(ptr) };
        // self owns the weak reference, don't allow it to be dropped
        let weak = ManuallyDrop::new(weak);
        weak.upgrade().unwrap()
    }

    /// # Safety
    ///
    /// Underlying type of self must be Rc<T>.
    #[inline]
    unsafe fn into_rc_unchecked<T>(self, ptr: *const T) -> Rc<T> {
        let ptr = ptr.map_addr(Self::to_object_addr);
        let _ = ManuallyDrop::new(self);
        unsafe { Rc::from_raw(ptr) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be rc::Weak<T>.
    #[inline]
    unsafe fn into_rc_weak_unchecked<T>(self, ptr: *const T) -> Weak<T> {
        let ptr = ptr.map_addr(Self::to_object_addr);
        let _ = ManuallyDrop::new(self);
        unsafe { Weak::from_raw(ptr) }
    }

    /// # Safety
    ///
    /// Underlying type of self must be Arc<T>.
    #[inline]
    unsafe fn into_arc_unchecked<T>(self, ptr: *const T) -> Arc<T> {
        let ptr = ptr.map_addr(Self::to_object_addr);
        let _ = ManuallyDrop::new(self);
        unsafe { Arc::from_raw(ptr) }
    }

    #[inline]
    fn is_object(&self, tag: usize) -> bool {
        self.as_raw() & Self::TAG_BITS == tag
    }

    #[inline]
    fn to_object_addr(addr: usize) -> usize {
        addr & !Self::TAG_BITS
    }

    #[inline]
    fn as_kind(&self) -> ValueKind {
        match self.get_tag() {
            Self::TAG_STRING => ValueKind::String,
            Self::TAG_FUNCTION => ValueKind::Function,
            Self::TAG_NATIVE_FN => ValueKind::NativeFn,
            Self::TAG_CLOSURE => ValueKind::Closure,
            Self::TAG_WEAK_CLOSURE => ValueKind::WeakClosure,
            Self::TAG_UPVALUE => ValueKind::Upvalue,
            Self::TAG_CLASS => ValueKind::Class,
            Self::TAG_INSTANCE => ValueKind::Instance,
            Self::TAG_WEAK_INSTANCE => ValueKind::WeakInstance,
            Self::TAG_BOUND_METHOD => ValueKind::BoundMethod,
            Self::TAG_NIL => ValueKind::Nil,
            Self::TAG_FALSE => ValueKind::False,
            Self::TAG_TRUE => ValueKind::True,
            _ => ValueKind::Number,
        }
    }

    #[inline]
    pub fn get_tag(&self) -> usize {
        self.as_raw() & Self::TAG_BITS
    }
    #[inline]
    pub fn into_weak(self) -> Self {
        match self.get_tag() {
            Self::TAG_CLOSURE => {
                let obj_ptr = unsafe { self.closure.map_addr(Self::to_object_addr) };
                let rc = unsafe { Rc::from_raw(obj_ptr) };
                let weak = Rc::downgrade(&rc);
                // forget self, so that we don't double drop Rc
                let _ = ManuallyDrop::new(self);
                Self::new_weak_closure(weak)
            }
            Self::TAG_INSTANCE => {
                let obj_ptr = unsafe { self.instance.map_addr(Self::to_object_addr) };
                let rc = unsafe { Rc::from_raw(obj_ptr) };
                let weak = Rc::downgrade(&rc);
                // forget self, so that we don't double drop Rc
                let _ = ManuallyDrop::new(self);
                Self::new_weak_instance(weak)
            }
            _ => panic!("Cannot convert {:?} to weak", self.as_kind()),
        }
    }

    #[inline]
    pub fn convert_to_weak(&mut self) {
        match self.get_tag() {
            Self::TAG_CLOSURE => {
                let obj_ptr = unsafe { self.closure.map_addr(Self::to_object_addr) };
                let rc = unsafe { Rc::from_raw(obj_ptr) };
                let _weak = ManuallyDrop::new(Rc::downgrade(&rc));
                let ptr = obj_ptr.map_addr(|addr| addr | Self::TAG_WEAK_CLOSURE);
                self.closure = ptr;
            }
            Self::TAG_INSTANCE => {
                let obj_ptr = unsafe { self.instance.map_addr(Self::to_object_addr) };
                let rc = unsafe { Rc::from_raw(obj_ptr) };
                let _weak = ManuallyDrop::new(Rc::downgrade(&rc));
                let ptr = obj_ptr.map_addr(|addr| addr | Self::TAG_WEAK_INSTANCE);
                self.instance = ptr;
            }
            _ => panic!("Cannot convert {:?} to weak", self.as_kind()),
        }
    }

    #[inline]
    pub fn is_falsey(&self) -> bool {
        self.as_raw() == Self::NIL_VALUE || self.as_raw() == Self::FALSE_VALUE
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        if let (Some(s), Some(o)) = (self.try_to_number(), other.try_to_number()) {
            return s == o;
        }

        self.as_raw() == other.as_raw()
    }
}

impl Clone for Value {
    fn clone(&self) -> Self {
        unsafe {
            match self.get_tag() {
                // SAFETY: we own all the allocations here, since we are alive, they are too
                Self::TAG_STRING => {
                    let obj_ptr = self.string.map_addr(Self::to_object_addr);
                    Arc::increment_strong_count(obj_ptr);

                    Self {
                        string: self.string,
                    }
                }

                Self::TAG_FUNCTION => {
                    let obj_ptr = self.function.map_addr(Self::to_object_addr);
                    Rc::increment_strong_count(obj_ptr);

                    Self {
                        function: self.function,
                    }
                }

                Self::TAG_NATIVE_FN => {
                    let obj_ptr = self.native_fn.map_addr(Self::to_object_addr);
                    Rc::increment_strong_count(obj_ptr);

                    Self {
                        native_fn: self.native_fn,
                    }
                }

                Self::TAG_CLOSURE => {
                    let obj_ptr = self.closure.map_addr(Self::to_object_addr);
                    Rc::increment_strong_count(obj_ptr);

                    Self {
                        closure: self.closure,
                    }
                }

                Self::TAG_WEAK_CLOSURE => {
                    let obj_ptr = self.closure.map_addr(Self::to_object_addr);
                    // There is no increment_weak_count method. But we can do it manually.
                    let orig = mem::ManuallyDrop::new(Weak::from_raw(obj_ptr));
                    let _new = mem::ManuallyDrop::new(orig.clone());
                    Self {
                        closure: self.closure,
                    }
                }

                Self::TAG_UPVALUE => {
                    let obj_ptr = self.upvalue.map_addr(Self::to_object_addr);
                    Rc::increment_strong_count(obj_ptr);

                    Self {
                        upvalue: self.upvalue,
                    }
                }

                Self::TAG_CLASS => {
                    let obj_ptr = self.class.map_addr(Self::to_object_addr);
                    Rc::increment_strong_count(obj_ptr);

                    Self { class: self.class }
                }

                Self::TAG_INSTANCE => {
                    let obj_ptr = self.instance.map_addr(Self::to_object_addr);
                    Rc::increment_strong_count(obj_ptr);

                    Self {
                        instance: self.instance,
                    }
                }

                Self::TAG_WEAK_INSTANCE => {
                    let obj_ptr = self.instance.map_addr(Self::to_object_addr);
                    // There is no increment_weak_count method. But we can do it manually.
                    let orig = mem::ManuallyDrop::new(Weak::from_raw(obj_ptr));
                    let _new = mem::ManuallyDrop::new(orig.clone());

                    Self {
                        instance: self.instance,
                    }
                }

                Self::TAG_BOUND_METHOD => {
                    let obj_ptr = self.bound_method.map_addr(Self::to_object_addr);
                    Rc::increment_strong_count(obj_ptr);
                    Self {
                        bound_method: self.bound_method,
                    }
                }
                Self::TAG_FALSE | Self::TAG_TRUE | Self::TAG_NIL => Self { raw: self.raw },

                _ => Self {
                    number: self.number,
                },
            }
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_number() {
            return write!(f, "{}", self.try_to_number().unwrap());
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
            Self::TAG_CLOSURE | Self::TAG_WEAK_CLOSURE => {
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
            Self::TAG_INSTANCE | Self::TAG_WEAK_INSTANCE => {
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

impl Drop for Value {
    fn drop(&mut self) {
        if self.is_number() {
            return;
        }

        //println!("dropping value {:?}", self.as_kind());
        unsafe {
            match self.get_tag() {
                Self::TAG_FALSE | Self::TAG_TRUE | Self::TAG_NIL => {}
                // SAFETY: reconstruct tha Rc/Arc/Weak that we were given ownership of
                //  We haven't given that ownership away thus it's valid to recreate it
                Self::TAG_STRING => {
                    let obj_ptr = self.string.map_addr(Self::to_object_addr);
                    Arc::from_raw(obj_ptr);
                }

                Self::TAG_FUNCTION => {
                    let obj_ptr = self.function.map_addr(Self::to_object_addr);
                    Rc::from_raw(obj_ptr);
                }

                Self::TAG_NATIVE_FN => {
                    let obj_ptr = self.native_fn.map_addr(Self::to_object_addr);
                    Rc::from_raw(obj_ptr);
                }

                Self::TAG_CLOSURE => {
                    let obj_ptr = self.closure.map_addr(Self::to_object_addr);
                    Rc::from_raw(obj_ptr);
                }

                Self::TAG_WEAK_CLOSURE => {
                    let obj_ptr = self.closure.map_addr(Self::to_object_addr);
                    Weak::from_raw(obj_ptr);
                }

                Self::TAG_UPVALUE => {
                    let obj_ptr = self.upvalue.map_addr(Self::to_object_addr);
                    Rc::from_raw(obj_ptr);
                }

                Self::TAG_CLASS => {
                    let obj_ptr = self.class.map_addr(Self::to_object_addr);
                    Rc::from_raw(obj_ptr);
                }

                Self::TAG_INSTANCE => {
                    let obj_ptr = self.instance.map_addr(Self::to_object_addr);
                    Rc::from_raw(obj_ptr);
                }

                Self::TAG_WEAK_INSTANCE => {
                    let obj_ptr = self.instance.map_addr(Self::to_object_addr);
                    Weak::from_raw(obj_ptr);
                }

                Self::TAG_BOUND_METHOD => {
                    let obj_ptr = self.bound_method.map_addr(Self::to_object_addr);
                    Rc::from_raw(obj_ptr);
                }
                _ => {}
            }
        }
    }
}

/// Reference to a value inside Rc<T>
///
/// This allows a &T inside an Rc<T> to be converted back to Rc<T>.
/// It's used to delay a clone of Rc<T> until it's needed.
#[derive(Debug, PartialEq)]
pub struct RcRef<'a, T> {
    data: *const T,
    marker: PhantomData<&'a Rc<T>>,
}

impl<'a, T> RcRef<'a, T> {
    /// SAFETY
    ///
    /// The data pointer must be obtained from `Rc::into_raw` and the underlying Rc must by alive for 'a
    unsafe fn new_unchecked(data: *const T) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }

    pub fn to_rc(&self) -> Rc<T> {
        // SAFETY: The safety requirements on type require that pointer is constructed from an Rc<T> which also must be alive.
        unsafe {
            Rc::increment_strong_count(self.data);
            Rc::from_raw(self.data)
        }
    }
}

impl<'a, T> Deref for RcRef<'a, T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.data }
    }
}

#[derive(Debug)]
pub enum CowRcRef<'a, T> {
    Owned(Rc<T>),
    Borrowed(RcRef<'a, T>),
}

impl<T> PartialEq for CowRcRef<'_, T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let a = self.deref();
        let b = other.deref();
        a.eq(b)
    }
}

impl<'a, T> CowRcRef<'a, T> {
    pub fn into_rc(self) -> Rc<T> {
        match self {
            Self::Owned(rc) => rc,
            Self::Borrowed(rc) => rc.to_rc(),
        }
    }
}

impl<'a, T> Deref for CowRcRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(rc) => rc,
            Self::Borrowed(rc) => rc,
        }
    }
}

/// Reference to a value inside Arc<T>
///
/// This allows a &T inside an Arc<T> to be converted back to Arc<T>.
/// It's used to delay a clone of Arc<T> until it's needed.
#[derive(Debug, PartialEq)]
pub struct ArcRef<'a, T> {
    data: *const T,
    marker: PhantomData<&'a Arc<T>>,
}

impl<'a, T> ArcRef<'a, T> {
    /// # Safety
    ///
    /// The pointer must be constructed from an Arc<T> which must stay alive for 'a.
    pub unsafe fn new_unchecked(data: *const T) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }

    pub fn new(data: &'a Arc<T>) -> Self {
        Self {
            data: Arc::as_ptr(data),
            marker: PhantomData,
        }
    }

    pub fn to_arc(&self) -> Arc<T> {
        unsafe {
            Arc::increment_strong_count(self.data);
            Arc::from_raw(self.data)
        }
    }
}

impl<'a, T> Deref for ArcRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        // SAFETY: the
        unsafe { &*self.data }
    }
}

impl<T> PartialEq<str> for ArcRef<'_, T>
where
    T: PartialEq<str>,
{
    fn eq(&self, other: &str) -> bool {
        let a = self.deref();
        let b = other;
        a.eq(b)
    }
}

#[derive(Debug, PartialEq)]
#[repr(u64)]
pub enum ValueKind {
    Number,
    String = Value::TAG_STRING as u64,
    Function = Value::TAG_FUNCTION as u64,
    NativeFn = Value::TAG_NATIVE_FN as u64,
    Closure = Value::TAG_CLOSURE as u64,
    WeakClosure = Value::TAG_WEAK_CLOSURE as u64,
    Upvalue = Value::TAG_UPVALUE as u64,
    Class = Value::TAG_CLASS as u64,
    Instance = Value::TAG_INSTANCE as u64,
    WeakInstance = Value::TAG_WEAK_INSTANCE as u64,
    BoundMethod = Value::TAG_BOUND_METHOD as u64,
    Nil = Value::TAG_NIL as u64,
    True = Value::TAG_TRUE as u64,
    False = Value::TAG_FALSE as u64,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct InternedString(pub Arc<InternedStringCore>);

impl ops::Deref for InternedString {
    type Target = InternedStringCore;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl InternedString {
    pub fn new(value: String) -> Self {
        Self(Arc::new(InternedStringCore::new(value)))
    }
}

#[derive(Debug)]
pub struct InternedStringCore {
    hash: u64,
    value: String,
}

impl InternedStringCore {
    pub fn new(value: String) -> Self {
        let hasher = BuildHasher::default();
        let hash = hasher.hash_one(&value);
        Self { hash, value }
    }

    pub fn get_hash(&self) -> u64 {
        self.hash
    }
}

impl PartialEq for InternedStringCore {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.value.as_ptr(), other.value.as_ptr())
    }
}

impl Eq for InternedStringCore {}

impl std::hash::Hash for InternedStringCore {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl fmt::Display for InternedString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for InternedStringCore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl ops::Deref for InternedStringCore {
    type Target = String;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Str<'a>(pub &'a str);

// impl Borrow<String> for InternedString {
//     fn borrow(&self) -> &String {
//         &self.0
//     }
// }

impl hashbrown::Equivalent<InternedString> for Str<'_> {
    fn equivalent(&self, key: &InternedString) -> bool {
        **key.0 == self.0
    }
}

impl Borrow<InternedStringCore> for InternedString {
    fn borrow(&self) -> &InternedStringCore {
        &self.0
    }
}

impl AsRef<str> for InternedStringCore {
    fn as_ref(&self) -> &str {
        &self.value
    }
}

impl From<InternedStringCore> for String {
    fn from(s: InternedStringCore) -> Self {
        s.value
    }
}

impl From<&InternedStringCore> for String {
    fn from(s: &InternedStringCore) -> Self {
        s.value.to_string()
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
            receiver: receiver.into_weak(),
            method,
        }
    }
}

#[cfg(test)]
mod tests {
    use core::f64;

    use super::*;

    fn test_number_inner(number: f64) -> Value {
        let v = Value::new_number(number);
        assert_eq!(v.as_kind(), ValueKind::Number);
        assert!(v.is_number());
        assert!(!v.is_nil());
        assert_eq!(v.try_to_bool(), None);
        assert_eq!(v.try_as_string(), None);
        assert_eq!(v.try_as_function(), None);
        assert_eq!(v.try_as_native_fn(), None);
        assert_eq!(v.try_as_closure(), None);
        assert_eq!(v.try_as_class(), None);
        assert_eq!(v.try_as_instance(), None);
        assert_eq!(v.try_as_bound_method(), None);
        assert_eq!(v.try_as_upvalue(), None);
        v
    }

    #[test]
    fn test_number() {
        let v = test_number_inner(1.5);
        assert_eq!(v.try_to_number(), Some(1.5));

        let v = test_number_inner(0.0);
        assert_eq!(v.try_to_number(), Some(0.0));

        let v = test_number_inner(f64::MIN);
        assert_eq!(v.try_to_number(), Some(f64::MIN));

        let v = test_number_inner(f64::MAX);
        assert_eq!(v.try_to_number(), Some(f64::MAX));

        let v = test_number_inner(f64::NAN);
        assert!(v.try_to_number().unwrap().is_nan());
        let v = test_number_inner(f64::INFINITY);
        assert!(v.try_to_number().unwrap().is_infinite());

        let v = test_number_inner(f64::NEG_INFINITY);
        assert!(v.try_to_number().unwrap().is_infinite());
    }

    proptest::proptest! {
        #![proptest_config(proptest::test_runner::Config::with_cases(1_000_000))]
        #[test]
        fn test_number_random(n in  proptest::num::f64::NORMAL | proptest::num::f64::SUBNORMAL | proptest::num::f64::ZERO | proptest::num::f64::INFINITE) {
            let v = test_number_inner(n);
            assert_eq!(v.try_to_number(), Some(n));
        }
    }

    #[test]
    fn test_nil() {
        let v = Value::NIL;
        assert_eq!(v.as_kind(), ValueKind::Nil);
        assert!(v.is_nil());
        assert_eq!(v.try_to_number(), None);
        assert_eq!(v.try_to_bool(), None);
        assert_eq!(v.try_as_string(), None);
        assert_eq!(v.try_as_function(), None);
        assert_eq!(v.try_as_native_fn(), None);
        assert_eq!(v.try_as_closure(), None);
        assert_eq!(v.try_as_class(), None);
        assert_eq!(v.try_as_instance(), None);
        assert_eq!(v.try_as_bound_method(), None);
        assert_eq!(v.try_as_upvalue(), None);
    }

    #[test]
    fn test_bool() {
        let v = Value::new_bool(true);
        assert_eq!(v.as_kind(), ValueKind::True);
        assert!(!v.is_nil());
        assert_eq!(v.try_to_number(), None);
        assert_eq!(v.try_to_bool(), Some(true));
        assert_eq!(v.try_as_string(), None);
        assert_eq!(v.try_as_function(), None);
        assert_eq!(v.try_as_native_fn(), None);
        assert_eq!(v.try_as_closure(), None);
        assert_eq!(v.try_as_class(), None);
        assert_eq!(v.try_as_instance(), None);
        assert_eq!(v.try_as_bound_method(), None);
        assert_eq!(v.try_as_upvalue(), None);

        let v = Value::new_bool(false);
        assert_eq!(v.as_kind(), ValueKind::False);
        assert!(!v.is_nil());
        assert_eq!(v.try_to_number(), None);
        assert_eq!(v.try_to_bool(), Some(false));
        assert_eq!(v.try_as_string(), None);
        assert_eq!(v.try_as_function(), None);
        assert_eq!(v.try_as_native_fn(), None);
        assert_eq!(v.try_as_closure(), None);
        assert_eq!(v.try_as_class(), None);
        assert_eq!(v.try_as_instance(), None);
        assert_eq!(v.try_as_bound_method(), None);
        assert_eq!(v.try_as_upvalue(), None);
    }

    #[test]
    fn test_string() {
        let s = InternedString::new("ads".into());
        let expected = s.clone();
        let v = Value::new_string(s);

        assert_eq!(v.as_kind(), ValueKind::String);
        assert!(!v.is_nil());
        assert_eq!(v.try_to_number(), None);
        assert_eq!(v.try_to_bool(), None);
        assert_eq!(v.try_as_string().unwrap().deref(), expected.deref());
        assert_eq!(v.try_as_function(), None);
        assert_eq!(v.try_as_native_fn(), None);
        assert_eq!(v.try_as_closure(), None);
        assert_eq!(v.try_as_class(), None);
        assert_eq!(v.try_as_instance(), None);
        assert_eq!(v.try_as_bound_method(), None);
        assert_eq!(v.try_as_upvalue(), None);
    }

    #[test]
    fn test_instance() {
        let c = ObjClass::new(InternedString::new("ads".into()));
        let s = ObjInstance::new(Rc::new(RefCell::new(c)));
        let expected = s.clone();
        let v = Value::new_instance(Rc::new(RefCell::new(s)));

        assert_eq!(v.as_kind(), ValueKind::Instance);
        assert!(!v.is_nil());
        assert_eq!(v.try_to_number(), None);
        assert_eq!(v.try_to_bool(), None);
        assert_eq!(v.try_as_string(), None);
        assert_eq!(v.try_as_function(), None);
        assert_eq!(v.try_as_native_fn(), None);
        assert_eq!(v.try_as_closure(), None);
        assert_eq!(v.try_as_class(), None);
        assert_eq!(
            v.try_as_instance().unwrap().deref().borrow().deref(),
            &expected
        );
        assert_eq!(v.try_as_bound_method(), None);
        assert_eq!(v.try_as_upvalue(), None);
    }

    #[test]
    fn simple_drop() {
        let c = ObjClass::new(InternedString::new("ads".into()));
        let rc = Rc::new(RefCell::new(c.clone()));
        let v = Value::new_class(rc.clone());

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        let v = v.try_into_class();

        drop(v);

        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 0);
    }

    #[test]
    fn clone_and_drop() {
        let c = ObjClass::new(InternedString::new("ads".into()));
        let rc = Rc::new(RefCell::new(c.clone()));
        let v = Value::new_class(rc.clone());

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        let v2 = v.clone();

        assert_eq!(Rc::strong_count(&rc), 3);
        assert_eq!(Rc::weak_count(&rc), 0);

        drop(v);

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        drop(v2);

        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 0);
    }

    #[test]
    fn get_clone_and_drop() {
        let c = ObjClass::new(InternedString::new("ads".into()));
        let rc = Rc::new(RefCell::new(c.clone()));
        let v = Value::new_class(rc.clone());

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        let v2 = v.try_as_class().unwrap();

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        let v2 = v2.to_rc();
        assert_eq!(Rc::strong_count(&rc), 3);
        assert_eq!(Rc::weak_count(&rc), 0);
        drop(v2);

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        drop(v);

        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 0);
    }

    #[test]
    fn drop_weak() {
        let c = ObjClass::new(InternedString::new("ads".into()));
        let i = ObjInstance::new(Rc::new(RefCell::new(c.clone())));
        let rc = Rc::new(RefCell::new(i.clone()));
        let v = Value::new_instance(rc.clone());

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        let weak = v.into_weak();
        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 1);

        drop(weak);

        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 0);
    }

    #[test]
    fn drop_weak2() {
        let c = ObjClass::new(InternedString::new("ads".into()));
        let i = ObjInstance::new(Rc::new(RefCell::new(c.clone())));
        let rc = Rc::new(RefCell::new(i.clone()));
        let mut v = Value::new_instance(rc.clone());

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        v.convert_to_weak();
        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 1);
    }

    #[test]
    fn drop_weak3() {
        let c = ObjClass::new(InternedString::new("ads".into()));
        let i = ObjInstance::new(Rc::new(RefCell::new(c.clone())));
        let rc = Rc::new(RefCell::new(i.clone()));
        let v = Value::new_instance(rc.clone());

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        let weak = v.into_weak();
        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 1);

        assert!(weak.is_weak_instance());

        let inst = weak.try_as_instance().unwrap();

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 1);

        let inst = inst.into_rc();
        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 1);
        drop(weak);

        assert_eq!(Rc::strong_count(&rc), 2);
        assert_eq!(Rc::weak_count(&rc), 0);

        drop(inst);

        assert_eq!(Rc::strong_count(&rc), 1);
        assert_eq!(Rc::weak_count(&rc), 0);
    }

    #[test]
    fn simple_drop_string() {
        let c = InternedString::new("ads".into());
        let rc = c.0.clone();
        let v = Value::new_string(c);

        assert_eq!(Arc::strong_count(&rc), 2);
        assert_eq!(Arc::weak_count(&rc), 0);

        drop(v);

        assert_eq!(Arc::strong_count(&rc), 1);
        assert_eq!(Arc::weak_count(&rc), 0);
    }

    #[test]
    fn clone_and_drop_string() {
        let c = InternedString::new("ads".into());
        let rc = c.0.clone();
        let v = Value::new_string(c);

        assert_eq!(Arc::strong_count(&rc), 2);
        assert_eq!(Arc::weak_count(&rc), 0);

        let v2 = v.clone();

        assert_eq!(Arc::strong_count(&rc), 3);
        assert_eq!(Arc::weak_count(&rc), 0);

        drop(v);

        assert_eq!(Arc::strong_count(&rc), 2);
        assert_eq!(Arc::weak_count(&rc), 0);

        drop(v2);

        assert_eq!(Arc::strong_count(&rc), 1);
        assert_eq!(Arc::weak_count(&rc), 0);
    }

    #[test]
    fn get_clone_and_drop_string() {
        let c = InternedString::new("ads".into());
        let rc = c.clone().0;
        let v = Value::new_string(c);

        assert_eq!(Arc::strong_count(&rc), 2);
        assert_eq!(Arc::weak_count(&rc), 0);

        let v2 = v.try_as_string().unwrap();

        assert_eq!(Arc::strong_count(&rc), 2);
        assert_eq!(Arc::weak_count(&rc), 0);

        let v2 = v2.to_arc();
        assert_eq!(Arc::strong_count(&rc), 3);
        assert_eq!(Arc::weak_count(&rc), 0);
        drop(v2);

        assert_eq!(Arc::strong_count(&rc), 2);
        assert_eq!(Arc::weak_count(&rc), 0);

        drop(v);

        assert_eq!(Arc::strong_count(&rc), 1);
        assert_eq!(Arc::weak_count(&rc), 0);
    }
}
