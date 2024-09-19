use core::slice;
use std::marker::PhantomData;
use std::{mem, ops};

use crate::value::Value;

pub const STACK_MAX: usize = 1024;

pub fn create_stack() -> [Value; STACK_MAX] {
    [Value::NIL; STACK_MAX]
}

#[derive(Debug)]
pub struct Stack<'a> {
    start: *mut Value,
    end: *mut Value,
    sp: *mut Value,
    marker: PhantomData<&'a mut [Value; STACK_MAX]>,
}

impl<'stack> Stack<'stack> {
    pub fn new(array: &'stack mut [Value; STACK_MAX]) -> Self {
        let array = array.as_mut_ptr_range();
        Self {
            start: array.start,
            end: array.end,
            sp: array.start,
            marker: PhantomData,
        }
    }

    #[inline]
    pub fn is_full(&self) -> bool {
        self.sp == self.end
    }

    #[inline]
    pub fn push(&mut self, value: Value) -> Result<(), ()> {
        if self.sp == self.end {
            return Err(());
        }

        unsafe {
            self.push_unchecked(value);
        }
        Ok(())
    }

    #[inline]
    pub unsafe fn push_unchecked(&mut self, value: Value) {
        unsafe {
            self.sp.write(value);
            self.sp = self.sp.add(1);
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        unsafe { self.sp.offset_from(self.start) as usize }
    }

    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.sp == self.start
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<&Value> {
        let ptr = unsafe { self.start.add(index) };
        if ptr >= self.sp {
            return None;
        }

        Some(unsafe { &*ptr })
    }

    #[inline]
    pub fn get_slice(&self, range: ops::RangeFrom<usize>) -> Option<&[Value]> {
        let start = unsafe { self.start.add(range.start) };
        if start >= self.sp {
            return None;
        }
        let len = unsafe { self.sp.offset_from(start) };
        assert!(len > 0);

        Some(unsafe { std::slice::from_raw_parts(start, len as usize) })
    }

    #[inline]
    pub fn get_mut(&mut self, index: usize) -> Option<&mut Value> {
        let ptr = unsafe { self.start.add(index) };
        if ptr >= self.sp {
            return None;
        }

        Some(unsafe { &mut *ptr })
    }

    #[inline]
    pub fn last(&self) -> Option<&Value> {
        if self.sp == self.start {
            return None;
        }

        Some(unsafe { &*self.sp.sub(1) })
    }

    pub fn swap_remove(&mut self, index: usize) -> Option<Value> {
        let ptr = unsafe { self.start.add(index) };
        if ptr >= self.sp {
            return None;
        }

        self.sp = unsafe { self.sp.sub(1) };

        unsafe { std::ptr::swap(ptr, self.sp) };
        Some(unsafe { std::ptr::read(self.sp) })
    }

    #[inline]
    pub fn pop(&mut self) -> Option<Value> {
        if self.sp == self.start {
            return None;
        }
        self.sp = unsafe { self.sp.sub(1) };
        let result = unsafe { std::ptr::read(self.sp) };
        //unsafe { std::ptr::write(self.sp, Value::Nil) }
        Some(result)
    }

    #[inline]
    pub fn pop2(&mut self) -> Option<(Value, Value)> {
        if self.len() < 2 {
            return None;
        }

        self.sp = unsafe { self.sp.sub(2) };

        Some(unsafe { (std::ptr::read(self.sp), std::ptr::read(self.sp.add(1))) })
    }

    #[inline]
    pub fn truncate(&mut self, len: usize) {
        assert!(len <= self.len());
        self.sp = unsafe { self.start.add(len) };
    }

    #[inline]
    pub fn pop_n(&mut self, count: usize) {
        assert!(count <= self.len());

        if count == 0 {
            return;
        }

        self.sp = unsafe { self.sp.sub(count) };
    }

    pub fn iter(&self) -> slice::Iter<'_, Value> {
        unsafe { slice::from_raw_parts(self.start, self.len()).iter() }
    }
}

impl ops::Index<usize> for Stack<'_> {
    type Output = Value;
    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        self.get(index).unwrap()
    }
}

impl ops::Index<ops::RangeFrom<usize>> for Stack<'_> {
    type Output = [Value];
    #[inline]
    fn index(&self, index: ops::RangeFrom<usize>) -> &Self::Output {
        self.get_slice(index).unwrap()
    }
}

impl ops::IndexMut<usize> for Stack<'_> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.get_mut(index).unwrap()
    }
}
