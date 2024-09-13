use core::panic;
use std::cell::{RefCell, RefMut};
use std::collections::BTreeMap;
use std::io::{self, Write};
use std::rc::Rc;
use std::sync::{Arc, LazyLock, Mutex};

use fnv::FnvBuildHasher;
use hashbrown::hash_map::{Entry, RawEntryMut, RawVacantEntryMut};
use hashbrown::{HashMap, HashSet};

use crate::bytecode::{BytesCursor, OpCode};
use crate::common::Span;
use crate::compiler;
use crate::value::{
    ArcRef, InternedString, InternedStringCore, NativeFn, ObjBoundMethod, ObjClass, ObjClosure,
    ObjInstance, ObjUpvalue,  Str, Value,
};

pub(crate) type BuildHasher = FnvBuildHasher;

#[cfg(feature = "debug_disassemble")]
use crate::disassembler::Disassembler;

pub static STRING_INTERNER: LazyLock<Mutex<StringInterner>> =
    LazyLock::new(|| Mutex::new(StringInterner::new()));

use self::error::{RuntimeError, RuntimeErrorKind};

pub mod error;

const MAX_FRAMES: usize = 64;
const STACK_MAX: usize = 1024;
type Stack<T> = arrayvec::ArrayVec<T, STACK_MAX>;

#[derive(Debug)]
pub struct Vm<OUT = std::io::Stdout, OUTERR = std::io::Stderr> {
    pub stack: Stack<Value>,
    pub globals: HashMap<InternedString, Value, BuildHasher>,
    pub call_frames: arrayvec::ArrayVec<CallFrame, MAX_FRAMES>,
    pub frame: CallFrame,
    pub open_upvalues: BTreeMap<usize, Rc<RefCell<ObjUpvalue>>>,

    output: OUT,
    outerr: OUTERR,

    #[cfg(feature = "debug_trace")]
    pub disassembler: Disassembler,
}

impl Vm {
    pub fn new() -> Self {
        Self::with_output(std::io::stdout(), std::io::stderr())
    }
}

impl<OUT, OUTERR> Vm<OUT, OUTERR> {
    pub fn with_output(output: OUT, outerr: OUTERR) -> Self {
        #[cfg(feature = "debug_trace")]
        let disassembler = Disassembler::new(ByteCode::new(), Vec::new());

        let mut vm = Vm {
            frame: CallFrame::new(),
            stack: Stack::new(),
            globals: HashMap::default(),
            call_frames: arrayvec::ArrayVec::new(),
            open_upvalues: BTreeMap::new(),
            output,
            outerr,

            #[cfg(feature = "debug_trace")]
            disassembler,
        };
        vm.define_native("clock", Self::native_clock);

        vm
    }

    #[allow(
        clippy::result_unit_err,
        reason = "it print all the errors to the output in self, but we still want to return if there was an error or not, this is clearer than returning a bool"
    )]
    pub fn compile(&mut self, input: &str) -> Result<(), ()>
    where
        for<'b> &'b mut OUTERR: io::Write,
    {
        let compiler = crate::compiler::Compiler::from_str(input);
        let (bytecode, constants) = match compiler.compile() {
            Ok(bytecode) => bytecode,
            Err(err) => {
                let report = miette::Report::new(err.to_owned());
                match writeln!(&mut self.outerr, "{report:?}") {
                    Ok(_) => (),
                    Err(_) => {
                        eprintln!("failed to report errors from vm, aborting ...");
                        std::process::abort();
                    }
                }
                return Err(());
            }
        };

        #[cfg(feature = "debug_trace")]
        {
            self.disassembler = Disassembler::new(bytecode.clone(), constants.clone());
        }

        self.frame.constants = constants.into();
        self.frame.spans = Rc::new(bytecode.spans);
        self.frame.instructions = BytesCursor::new(bytecode.code.into());
        self.push(Value::NIL).unwrap();
        self.frame.slots = 0;

        Ok(())
    }

    pub fn run(&mut self, input: &str)
    where
        for<'b> &'b mut OUT: io::Write,
        for<'b> &'b mut OUTERR: io::Write,
    {
        if let Err(err) = self.run_core() {
            let report = miette::Report::new(err).with_source_code(input.to_string());
            match writeln!(&mut self.outerr, "{report:?}") {
                Ok(_) => (),
                Err(_) => {
                    eprintln!("failed to report errors from vm, aborting ...");
                    std::process::abort();
                }
            }
        }
    }

    #[inline]
    fn push(&mut self, value: Value) -> Result<(), RuntimeError> {
        match self.stack.try_push(value) {
            Ok(_) => Ok(()),
            Err(_) => {
                let kind = RuntimeErrorKind::Msg("stack overflow".into());
                Err(self.runtime_error(kind, 1))
            }
        }
    }

    fn run_core(&mut self) -> Result<(), RuntimeError>
    where
        for<'b> &'b mut OUT: io::Write,
    {
        while let Some(op) = self.frame.instructions.try_u8().map(OpCode::from_u8) {
            #[cfg(feature = "debug_trace")]
            {
                print!("\n/ [");
                for slot in self.stack.iter().rev() {
                    print!("/   {slot}");
                }
                println!("/ ]");
                print!("/ ");
                self.disassembler.print_next();
            }

            match op {
                OpCode::Return => self.run_op_return()?,
                OpCode::Constant => self.run_op_constant()?,

                OpCode::DefineGlobal => self.run_op_define_global(),
                OpCode::GetGlobal => self.run_op_get_global()?,
                OpCode::SetGlobal => self.run_op_set_global()?,

                OpCode::GetLocal => self.run_op_get_local()?,
                OpCode::SetLocal => self.run_op_set_local(),

                OpCode::GetUpvalue => self.run_on_get_upvalue()?,
                OpCode::SetUpvalue => self.run_op_set_upvalue(),

                OpCode::GetProperty => self.run_op_get_property()?,
                OpCode::SetProperty => self.run_op_set_property()?,

                OpCode::Negate => self.run_op_negate()?,
                OpCode::Not => self.run_op_not()?,

                OpCode::Add => self.run_binary_add()?,
                OpCode::Subtract => self.binary_arithmetic_op(Self::subtract_number)?,
                OpCode::Multiply => self.binary_arithmetic_op(Self::multiply_number)?,
                OpCode::Divide => self.binary_arithmetic_op(Self::divide_number)?,

                OpCode::Eq => self.run_op_eq()?,
                OpCode::Lt => self.binary_cmp_op(Self::lt_number)?,
                OpCode::Gt => self.binary_cmp_op(Self::gt_number)?,

                OpCode::Nil => self.push(Value::NIL)?,
                OpCode::True => self.push(Value::TRUE)?,
                OpCode::False => self.push(Value::FALSE)?,

                OpCode::Print => self.run_op_print(),
                OpCode::Pop => {
                    self.stack.pop();
                }

                OpCode::JumpIfFalse => self.run_op_jump_if_false(),
                OpCode::Jump => self.run_op_jump(),

                OpCode::Loop => self.run_op_loop(),
                OpCode::Call => self.run_op_call()?,
                OpCode::Closure => self.run_op_closure()?,
                OpCode::CloseUpvalue => self.run_op_close_upvalue(),

                OpCode::Class => self.run_op_class()?,
                OpCode::Method => self.run_op_method(),
                OpCode::Invoke => self.run_op_invoke()?,
                OpCode::Inherit => self.run_op_inherit()?,
                OpCode::GetSuper => self.run_op_get_super()?,
            }
        }

        Ok(())
    }

    #[inline]
    fn run_op_return(&mut self) -> Result<(), RuntimeError> {
        let result = self.stack.pop().expect("expected a return value on stack");
        self.close_upvalues(self.frame.slots);

        self.stack.truncate(self.frame.slots);

        self.push(result)?;
        self.frame = self
            .call_frames
            .pop()
            .expect("expected a previous call frame to exist when returning from function");

        Ok(())
    }

    #[inline]
    fn run_op_constant(&mut self) -> Result<(), RuntimeError> {
        let value = self.frame.expect_constant().clone();
        self.push(value)
    }

    fn run_op_define_global(&mut self) {
        let name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to define global with non string identifier");
        let value = self
            .stack
            .pop()
            .expect("tried to define global with no value on stack");

        let entry = self
            .globals
            .raw_entry_mut()
            .from_key_hashed_nocheck(name.get_hash(), &*name);
        match entry {
            RawEntryMut::Vacant(entry) => {
                entry.insert_hashed_nocheck(name.get_hash(), InternedString(name.to_arc()), value);
            }
            RawEntryMut::Occupied(mut entry) => {
                entry.insert(value);
            }
        }
    }

    fn run_op_get_global(&mut self) -> Result<(), RuntimeError> {
        let name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to get global with non string identifier");

        let raw_entry = self
            .globals
            .raw_entry()
            .from_key_hashed_nocheck(name.get_hash(), &*name);

        if let Some((_, value)) = raw_entry {
            self.push(value.clone())
        } else {
            let kind = RuntimeErrorKind::UndefinedVariable {
                name: InternedString(name.to_arc()),
            };
            Err(self.runtime_error(kind, 2))
        }
    }

    fn run_op_set_global(&mut self) -> Result<(), RuntimeError> {
        let name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to set global with non string identifier");

        let entry = self
            .globals
            .raw_entry_mut()
            .from_key_hashed_nocheck(name.get_hash(), &*name);

        match entry {
            RawEntryMut::Occupied(mut entry) => {
                let value = self
                    .stack
                    .last()
                    .expect("tried to set global with no value on the stack")
                    .clone();
                entry.insert(value);
            }
            RawEntryMut::Vacant(_) => {
                let kind = RuntimeErrorKind::UndefinedVariable {
                    name: InternedString(name.to_arc()),
                };
                return Err(self.runtime_error(kind, 2));
            }
        }
        Ok(())
    }

    #[inline]
    fn run_op_get_local(&mut self) -> Result<(), RuntimeError> {
        let value = self.expect_local().clone();
        self.push(value)
    }

    #[inline]
    fn run_op_set_local(&mut self) {
        *self.expect_local() = self
            .stack
            .last()
            .expect("expected value on stack to set local variable")
            .clone();
    }

    fn run_on_get_upvalue(&mut self) -> Result<(), RuntimeError> {
        let upvalue = self.frame.expect_upvalue();
        let value = match &*upvalue {
            ObjUpvalue::Open(stack_idx) => self
                .stack
                .get(*stack_idx)
                .expect("tried to get upvalue at invalid stack index")
                .clone(),
            ObjUpvalue::Closed(value) => value.clone(),
        };
        drop(upvalue);
        self.push(value)
    }

    fn run_op_set_upvalue(&mut self) {
        let mut upvalue = self.frame.expect_upvalue();
        let value = self
            .stack
            .last()
            .expect("tried to set upvalue without value on stack")
            .clone();
        match &mut *upvalue {
            ObjUpvalue::Open(stack_idx) => {
                let stack_slot = self
                    .stack
                    .get_mut(*stack_idx)
                    .expect("tried to set upvalue at invalid stack index");
                *stack_slot = value;
            }
            ObjUpvalue::Closed(upvalue) => {
                *upvalue = value;
            }
        }
    }

    fn run_op_get_property(&mut self) -> Result<(), RuntimeError> {
        // Stack: bottom, .., instance

        let Some(instance) = self
            .stack
            .last()
            .expect("expected instance on stack to get property")
            .try_as_instance()
        else {
            let kind = RuntimeErrorKind::Msg("only instances have properties".into());
            return Err(self.runtime_error(kind, 1));
        };
        let instance = instance.borrow();
        let prop_name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to get property name from non string");

        let prop_value = instance
            .properties
            .raw_entry()
            .from_key_hashed_nocheck(prop_name.get_hash(), &*prop_name);

        match prop_value {
            Some((_, value)) => {
                let value = value.clone();
                drop(instance);
                self.stack.pop(); // instance
                self.push(value)
            }
            None => {
                let class = instance.class.borrow();

                let method = class
                    .methods
                    .raw_entry()
                    .from_key_hashed_nocheck(prop_name.get_hash(), &*prop_name);
                let Some((_, method)) = method else {
                    let kind = RuntimeErrorKind::UndefinedProperty {
                        name: InternedString(prop_name.to_arc()),
                    };
                    return Err(self.runtime_error(kind, 1));
                };
                let method = method.try_to_closure().unwrap();
                drop(class);
                drop(instance);

                let receiver = self.stack.pop().expect("expected receiver on stack");
                let method = ObjBoundMethod::new(receiver, method);
                let method = Value::new_bound_method(Rc::new(method));

                self.push(method)
            }
        }
    }

    fn run_op_set_property(&mut self) -> Result<(), RuntimeError> {
        // Stack: bottom, .., instance, value_to_set

        let instance = self.stack.swap_remove(self.stack.len() - 2);
        let Some(instance) = instance.try_as_instance() else {
            let kind = RuntimeErrorKind::Msg("only instances have properties".into());
            return Err(self.runtime_error(kind, 1));
        };
        let name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to set property name from non string");
        let value = self
            .stack
            .last()
            .expect("expected value on the stack to set property")
            .clone();

        let mut instance = instance.borrow_mut();
        let properties = &mut instance.properties;

        // Use raw_entry_mut to avoid rehashing and cloning the name for existing properties
        // It's quite common operation to set the same property multiple times
        let property = properties
            .raw_entry_mut()
            .from_key_hashed_nocheck(name.get_hash(), &*name);

        match property {
            hashbrown::hash_map::RawEntryMut::Occupied(mut entry) => {
                entry.insert(value);
            }
            hashbrown::hash_map::RawEntryMut::Vacant(entry) => {
                entry.insert_hashed_nocheck(name.get_hash(), InternedString(name.to_arc()), value);
            }
        }

        Ok(())
    }

    fn run_op_negate(&mut self) -> Result<(), RuntimeError> {
        let value = self
            .stack
            .pop()
            .expect("tried to negate with no value on stack");

        if value.is_number() {
            let v = value.try_to_number().unwrap();
            self.push(Value::new_number(-v))
        } else {
            let kind = RuntimeErrorKind::InvalidOperand { expected: "number" };
            Err(self.runtime_error(kind, 1))
        }
    }

    fn run_op_not(&mut self) -> Result<(), RuntimeError> {
        let value = self
            .stack
            .pop()
            .expect("tried to not with no value on stack");

        self.push(Value::new_bool(value.is_falsey()))
    }

    fn run_op_eq(&mut self) -> Result<(), RuntimeError> {
        let rhs = self
            .stack
            .pop()
            .expect("tried to eq with no rhs value on stack");
        let lhs = self
            .stack
            .pop()
            .expect("tried to eq with no lhs value on stack");

        self.push(Value::new_bool(lhs == rhs))
    }

    fn run_op_print(&mut self)
    where
        for<'b> &'b mut OUT: io::Write,
    {
        let value = self
            .stack
            .pop()
            .expect("tried to print with no value on stack");

        match writeln!(&mut self.output, "{}", value) {
            Ok(_) => (),
            Err(_) => {
                eprintln!("failed to write to output, aborting ...");
                std::process::abort();
            }
        }
    }

    fn run_op_jump_if_false(&mut self) {
        let offset = self
            .frame
            .instructions
            .try_u16()
            .expect("expected an u16 operand for op jump_if_false");

        if self
            .stack
            .last()
            .expect("tried a conditional jump with no value on stack")
            .is_falsey()
        {
            self.frame.instructions.jump_forward(offset as usize);
        }
    }

    #[inline]
    fn run_op_jump(&mut self) {
        let offset = self
            .frame
            .instructions
            .try_u16()
            .expect("expected an u16 operand for op jump");

        self.frame.instructions.jump_forward(offset as usize);
    }

    fn run_op_loop(&mut self) {
        let offset = self
            .frame
            .instructions
            .try_u16()
            .expect("expected an u16 operand for op loop");

        self.frame.instructions.jump_backward(offset as usize);
    }

    fn run_op_call(&mut self) -> Result<(), RuntimeError> {
        // Stack: bottom, .., callee, arg0, arg1, .., argN

        let arg_count = self
            .frame
            .instructions
            .try_u8()
            .expect("expected an u8 operand for op call");
        let callee = self
            .stack
            .get(self.stack.len() - arg_count as usize - 1)
            .expect("expected callee on stack")
            .clone();
        self.call_value(callee, arg_count)?;
        Ok(())
    }

    fn run_op_closure(&mut self) -> Result<(), RuntimeError> {
        let fun = self
            .frame
            .expect_constant()
            .try_as_function()
            .expect("expected a function on stack for op closure")
            .to_rc();

        #[cfg(feature = "debug_disassemble")]
        {
            println!("\n\n<fn {}>", &fun.name);
            let disassembler = Disassembler::new(
                fun.bytecode.clone(),
                fun.spans.clone(),
                self.frame.constants.clone(),
            );
            disassembler.print();
        }

        let mut upvalues = Vec::with_capacity(fun.upvalues_count);
        for _ in 0..fun.upvalues_count {
            let is_local = self
                .frame
                .instructions
                .try_u8()
                .expect("expected an u8 operand for op closure upvalue");
            let index = self
                .frame
                .instructions
                .try_u8()
                .expect("expected a second u8 operand for op closure upvalue");

            let upvalue = if is_local == 1 {
                self.capture_upvalue(self.frame.slots + index as usize)
            } else {
                Rc::clone(
                    self.frame
                        .closure
                        .as_ref()
                        .expect("expected to be inside closure to have upvalues")
                        .upvalues
                        .get(index as usize)
                        .expect("tried to get upvalue at invalid index"),
                )
            };

            upvalues.push(upvalue);
        }

        let closure = Value::new_closure(Rc::new(ObjClosure::new(fun, upvalues)));
        self.push(closure)
    }

    fn run_op_close_upvalue(&mut self) {
        self.close_upvalues(self.stack.len() - 1);
        self.stack.pop();
    }

    fn run_op_class(&mut self) -> Result<(), RuntimeError> {
        let name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to get class name from non string")
            .to_arc();

        let class = Value::new_class(Rc::new(RefCell::new(ObjClass::new(InternedString(name)))));
        self.push(class)
    }

    fn run_op_method(&mut self) {
        // Stack: bottom, .., class, method

        let name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to get method name from non string")
            .to_arc();
        let method = self
            .stack
            .pop()
            .expect("expected value to be on stack to be defined as class method");
        let class = self
            .stack
            .last()
            .expect("expected class to be on stack to define method on")
            .try_as_class()
            .expect("tried to define a method on non class");

        if !method.is_closure() {
            panic!("expected a closure on stack to be defined as class method");
        }

        let methods = &mut class.borrow_mut().methods;
        let method_entry = methods
            .raw_entry_mut()
            .from_key_hashed_nocheck(name.get_hash(), &*name);
        match method_entry {
            RawEntryMut::Vacant(entry) => {
                entry.insert_hashed_nocheck(name.get_hash(), InternedString(name), method);
            }
            RawEntryMut::Occupied(mut entry) => {
                entry.insert(method);
            }
        }
    }

    fn run_op_invoke(&mut self) -> Result<(), RuntimeError> {
        let idx = self
            .frame
            .instructions
            .try_u8()
            .expect("tried to read constant with no more instructions");

        let name = self
            .frame
            .constants
            .get(idx as usize)
            .expect("tried to get constant at invalid index")
            .try_as_string()
            .expect("tried to get method name from non string");
        let arg_count = self
            .frame
            .instructions
            .try_u8()
            .expect("expected a second u8 operand for op invoke");

        // TODO: remove this technically unnecessary clone, but borrow checker complains atm, because we are borrowing from self
        let name = name.to_arc();
        let name_ref = ArcRef::new(&name);
        self.invoke(name_ref, arg_count)?;
        Ok(())
    }

    fn run_op_inherit(&mut self) -> Result<(), RuntimeError> {
        // Stack: bottom, .., superclass, subclass

        let class = self
            .stack
            .pop()
            .expect("expected value on stack for op inherit");
        let class = class.try_as_class().expect("expected subclass on stack");
        let Some(super_class) = self
            .stack
            .last()
            .expect("expected superclass on stack")
            .try_as_class()
        else {
            let kind = RuntimeErrorKind::Msg("superclass must be a class".into());
            return Err(self.runtime_error(kind, 1));
        };

        class.borrow_mut().methods.extend(
            super_class
                .borrow()
                .methods
                .iter()
                .map(|(k, v)| (k.clone(), v.clone())),
        );

        Ok(())
    }

    fn run_op_get_super(&mut self) -> Result<(), RuntimeError> {
        // Stack: bottom, .., superclass

        let method_name = self
            .frame
            .expect_constant()
            .try_as_string()
            .expect("tried to get method name from non string")
            .to_arc();
        let method_name = ArcRef::new(&method_name);
        let superclass = self
            .stack
            .pop()
            .expect("expected value on stack for op get super");
        let superclass = superclass
            .try_as_class()
            .expect("expected superclass on stack");
        let superclass = superclass.borrow();
        self.bind_method(&superclass, method_name)?;
        Ok(())
    }

    fn invoke(
        &mut self,
        name: ArcRef<'_, InternedStringCore>,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let Some(instance) =
            self.stack[self.stack.len() - arg_count as usize - 1].try_as_instance()
        else {
            let kind = RuntimeErrorKind::Msg("only instances have methods".into());
            return Err(self.runtime_error(kind, 1));
        };

        let instance = instance.borrow();

        let field = instance
            .properties
            .raw_entry()
            .from_key_hashed_nocheck(name.get_hash(), &*name);

        if let Some((_, field)) = field {
            let idx = self.stack.len() - arg_count as usize - 1;
            let field = field.clone();
            drop(instance);
            self.stack[idx] = field.clone();
            return self.call_value(field, arg_count);
        }

        let class = instance.class.borrow();
        let method = class
            .methods
            .raw_entry()
            .from_key_hashed_nocheck(name.get_hash(), &*name);

        let Some((_, method)) = method else {
            let kind = RuntimeErrorKind::UndefinedProperty {
                name: InternedString(name.to_arc()),
            };
            return Err(self.runtime_error(kind, 1));
        };

        let method = method.try_to_closure().unwrap();
        drop(class);
        drop(instance);
        self.call_closure(method, arg_count)
    }

    fn invoke_from_class(
        &mut self,
        class: &ObjClass,
        name: &InternedString,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let method = class
            .methods
            .raw_entry()
            .from_key_hashed_nocheck(name.get_hash(), name);

        let Some((_, method)) = method else {
            let kind = RuntimeErrorKind::UndefinedProperty { name: name.clone() };
            return Err(self.runtime_error(kind, 1));
        };

        let method = method.try_to_closure().unwrap();

        self.call_closure(method, arg_count)
    }

    fn bind_method(
        &mut self,
        class: &ObjClass,
        name: ArcRef<'_, InternedStringCore>,
    ) -> Result<(), RuntimeError> {
        let method = class
            .methods
            .raw_entry()
            .from_key_hashed_nocheck(name.get_hash(), &*name);

        let Some((_, method)) = method else {
            let kind = RuntimeErrorKind::UndefinedProperty {
                name: InternedString(name.to_arc()),
            };
            return Err(self.runtime_error(kind, 1));
        };
        let method = method.try_to_closure().unwrap();

        let receiver = self.stack.pop().expect("expected receiver on stack");
        let method = ObjBoundMethod::new(receiver, method);
        let method = Value::new_bound_method(Rc::new(method));

        self.push(method)
    }

    fn close_upvalues(&mut self, idx: usize) {
        while let Some(last) = self.open_upvalues.last_entry() {
            if *last.key() >= idx {
                // Make sure not to create a Rc cycle
                let mut value = self.stack[*last.key()].clone();

                match value.get_tag() {
                    Value::TAG_CLASS => {
                        // One way to create cycle is if upvalue is a class and it's methods hold these upvalues
                        // Since we are closing the upvalue of class, then the class must be leaving the stack.
                        // This means that the closure should become the owner of the class and the class should not have a strong reference to that closure.
                        // Essentially the user program doesn't have a reference to the class anymore, only to the closure.
                        //
                        // Note that we cannot make all closure references from class weak because if the closure calls other closures, the class must still own them,
                        // or they will be removed.
                        //
                        // In summary, we must make all references from the class to the closure weak, where the closure has an upvalue to the class.
                        let cls = value.try_as_class().unwrap();
                        let mut class = cls.borrow_mut();

                        for m in class.methods.values_mut() {
                            let method = m.try_as_closure().unwrap();
                            let mut has_self_ref = false;
                            for u in method.upvalues.iter() {
                                let u = u.borrow();
                                if let ObjUpvalue::Open(idx_) = &*u {
                                    if *idx_ == idx {
                                        has_self_ref = true;
                                        break;
                                    }
                                }
                            }

                            if has_self_ref {
                                m.convert_to_weak();
                            }
                        }
                    }
                    Value::TAG_CLOSURE => {
                        // A self referential closure will have an upvalue that points to itself, that upvalue needs to be a weak reference
                        let closure = value.try_as_closure().unwrap();
                        let mut has_self_ref = false;
                        for u in closure.upvalues.iter() {
                            let u = u.borrow();
                            if let ObjUpvalue::Open(idx_) = &*u {
                                if *idx_ == idx {
                                    has_self_ref = true;
                                    break;
                                }
                            }
                        }

                        if has_self_ref {
                            value = value.into_weak();
                        }
                    }
                    _ => {}
                }

                *last.get().borrow_mut() = ObjUpvalue::Closed(value);
                last.remove();
            } else {
                break;
            }
        }
    }

    fn capture_upvalue(&mut self, index: usize) -> Rc<RefCell<ObjUpvalue>> {
        for (stack_idx, upvalue) in self.open_upvalues.iter().rev() {
            if *stack_idx == index {
                return Rc::clone(upvalue);
            }

            if *stack_idx < index {
                break;
            }
        }

        let upvalue = Rc::new(RefCell::new(ObjUpvalue::Open(index)));
        self.open_upvalues.insert(index, Rc::clone(&upvalue));
        upvalue
    }

    fn call_value(&mut self, callee: Value, arg_count: u8) -> Result<(), RuntimeError> {
        match callee.get_tag() {
            Value::TAG_CLOSURE => {
                let fun = unsafe { callee.into_closure_unchecked() };
                self.call_closure(fun, arg_count)
            }
            //Value::Object(Object::Function(fun)) => self.call_function(&fun, arg_count),
            Value::TAG_NATIVE_FN => {
                let fun = unsafe { callee.into_native_fn_unchecked() };
                let result = fun(
                    arg_count,
                    &self.stack[self.stack.len() - arg_count as usize..],
                );
                self.stack
                    .truncate(self.stack.len() - arg_count as usize - 1);
                self.push(result)
            }
            Value::TAG_CLASS => {
                let cls = unsafe { callee.into_class_unchecked() };

                let instance = ObjInstance::new(Rc::clone(&cls));

                let instance = Value::new_instance(Rc::new(RefCell::new(instance)));
                let receiver_slot = self.stack.len() - arg_count as usize - 1;
                self.stack[receiver_slot] = instance;
                let class = cls.borrow();

                let initializer = class.methods.raw_entry().from_key_hashed_nocheck(
                    compiler::INIT_METHOD_NAME.get_hash(),
                    &*compiler::INIT_METHOD_NAME,
                );

                if let Some((_, initializer)) = initializer {
                    let initializer = initializer.try_to_closure().unwrap();
                    self.call_closure(initializer, arg_count)?;
                } else if arg_count != 0 {
                    let kind = RuntimeErrorKind::Msg(
                        format!("expected 0 arguments but got {}", arg_count).into(),
                    );
                    return Err(self.runtime_error(kind, 2));
                }

                // self.stack
                //     .truncate(self.stack.len() - arg_count as usize - 1);
                // self.push(instance);
                Ok(())
            }
            Value::TAG_BOUND_METHOD => {
                let method = unsafe { callee.into_bound_method_unchecked() };
                let receiver_slot = self.stack.len() - arg_count as usize - 1;
                self.stack[receiver_slot] = method.receiver.clone();
                self.call_closure(Rc::clone(&method.method), arg_count)
            }

            _ => {
                let kind = RuntimeErrorKind::Msg("can only call functions and classes".into());
                Err(self.runtime_error(kind, 2))
            }
        }
    }

    fn define_native(&mut self, name: impl Into<String> + AsRef<str>, fun: NativeFn) {
        let name = STRING_INTERNER.lock().unwrap().intern(name);
        let fun = Rc::new(fun);
        let fun = Value::new_native_fn(fun);
        self.globals.insert(name, fun);
    }

    fn native_clock(_arg_count: u8, _args: &[Value]) -> Value {
        let time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("expected current time to be after unix epoch")
            .as_secs_f64();

        Value::new_number(time)
    }

    fn call_closure(&mut self, closure: Rc<ObjClosure>, arg_count: u8) -> Result<(), RuntimeError> {
        let fun = &*closure.fun;

        if arg_count as usize != fun.arity {
            let kind = RuntimeErrorKind::WrongNumberOfArguments {
                expected: fun.arity,
                got: arg_count as usize,
            };
            return Err(self.runtime_error(kind, 2));
        }

        // println!("Calling function {}", fun.name);
        // let disassembler = Disassembler::new(fun.bytecode.clone(), self.constants.clone());
        // disassembler.print();

        let frame = CallFrame {
            // TODO: avoid cloning instructions
            instructions: BytesCursor::new(fun.bytecode.clone()),
            slots: self.stack.len() - arg_count as usize - 1,
            spans: Rc::clone(&fun.spans),
            constants: Rc::clone(&fun.constants),
            closure: Some(closure),
        };
        let prev_frame = std::mem::replace(&mut self.frame, frame);
        self.call_frames.push(prev_frame);

        Ok(())
    }

    // fn call_function(&mut self, fun: &ObjFunction, arg_count: u8) -> Result<(), RuntimeError> {
    //     if arg_count as usize != fun.arity {
    //         let kind = RuntimeErrorKind::WrongNumberOfArguments {
    //             expected: fun.arity,
    //             got: arg_count as usize,
    //         };
    //         return Err(self.runtime_error(kind, 2));
    //     }

    //     // println!("Calling function {}", fun.name);
    //     // let disassembler = Disassembler::new(fun.bytecode.clone(), self.constants.clone());
    //     // disassembler.print();

    //     let frame = CallFrame {
    //         // TODO: avoid cloning instructions
    //         instructions: BytesCursor::new(fun.bytecode.clone()),
    //         slots: self.stack.len() - arg_count as usize - 1,
    //         spans: Rc::clone(&fun.spans),
    //         constants: Rc::clone(&fun.constants),
    //     };
    //     let prev_frame = std::mem::replace(&mut self.frame, frame);
    //     self.call_frames.push(prev_frame);

    //     Ok(())
    // }

    fn run_binary_add(&mut self) -> Result<(), RuntimeError> {
        let op = Self::add_number;
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

        if let (Some(lhs), Some(rhs)) = (lhs.try_to_number(), rhs.try_to_number()) {
            return self.push(Value::new_number(op(lhs, rhs)));
        }

        match (lhs.get_tag(), rhs.get_tag()) {
            (Value::TAG_STRING, Value::TAG_STRING) => {
                let lhs = lhs.try_as_string().unwrap();
                let rhs = rhs.try_as_string().unwrap();
                let new = lhs.to_string() + &rhs;
                let new = InternedStringCore::new(new);
                let new = STRING_INTERNER.lock().unwrap().intern(new);
                self.push(Value::new_string(new))
            }
            _ => {
                let kind = RuntimeErrorKind::InvalidOperands {
                    expected: "two numbers or string",
                };
                Err(self.runtime_error(kind, 1))
            }
        }
    }

    #[inline]
    fn binary_arithmetic_op(&mut self, op: impl Fn(f64, f64) -> f64) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        match (lhs.try_to_number(), rhs.try_to_number()) {
            (Some(lhs), Some(rhs)) => self.push(Value::new_number(op(lhs, rhs))),
            _ => {
                let kind = RuntimeErrorKind::InvalidOperands { expected: "number" };
                Err(self.runtime_error(kind, 1))
            }
        }
    }

    #[inline]
    fn binary_cmp_op(&mut self, op: impl Fn(f64, f64) -> bool) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();
        match (lhs.try_to_number(), rhs.try_to_number()) {
            (Some(lhs), Some(rhs)) => self.push(Value::new_bool(op(lhs, rhs))),
            _ => {
                let kind = RuntimeErrorKind::InvalidOperands { expected: "number" };
                Err(self.runtime_error(kind, 1))
            }
        }
    }

    #[inline]
    fn lt_number(lhs: f64, rhs: f64) -> bool {
        lhs < rhs
    }

    #[inline]
    fn gt_number(lhs: f64, rhs: f64) -> bool {
        lhs > rhs
    }

    #[inline]
    fn add_number(lhs: f64, rhs: f64) -> f64 {
        lhs + rhs
    }

    #[inline]
    fn subtract_number(lhs: f64, rhs: f64) -> f64 {
        lhs - rhs
    }

    #[inline]
    fn multiply_number(lhs: f64, rhs: f64) -> f64 {
        lhs * rhs
    }

    #[inline]
    fn divide_number(lhs: f64, rhs: f64) -> f64 {
        lhs / rhs
    }

    fn runtime_error(&self, kind: RuntimeErrorKind, offset: usize) -> RuntimeError {
        let span = self
            .frame
            .spans
            .get(&(self.frame.instructions.offset() - offset))
            .cloned()
            .map(|span| span.into());
        RuntimeError { kind, span }
    }

    /// Expects that next byte in instructions is a local index and reads the local
    fn expect_local(&mut self) -> &mut Value {
        let idx = self
            .frame
            .instructions
            .try_u8()
            .expect("tried to read local with no more instructions");
        self.stack
            .get_mut(self.frame.slots + idx as usize)
            .expect("tried to get local at invalid index")
    }
}

#[derive(Debug)]
pub struct CallFrame {
    pub instructions: BytesCursor,
    pub slots: usize,
    pub spans: Rc<HashMap<usize, Span, BuildHasher>>,
    pub constants: Rc<[Value]>,
    pub closure: Option<Rc<ObjClosure>>,
}

impl CallFrame {
    pub fn new() -> Self {
        Self {
            instructions: BytesCursor::new(Vec::new().into()),
            slots: 0,
            spans: Rc::new(HashMap::default()),
            constants: Rc::new([]),
            closure: None,
        }
    }

    /// Expects that next byte in instructions is a constant index and reads the constant
    fn expect_constant(&mut self) -> &Value {
        let idx = self
            .instructions
            .try_u8()
            .expect("tried to read constant with no more instructions");
        self.constants
            .get(idx as usize)
            .expect("tried to get constant at invalid index")
    }

    fn expect_upvalue(&mut self) -> RefMut<'_, ObjUpvalue> {
        let upvalue_index = self
            .instructions
            .try_u8()
            .expect("expected an u8 operand for op get upvalue");

        let closure = self
            .closure
            .as_ref()
            .expect("tried to get upvalue without closure");

        closure
            .upvalues
            .get(upvalue_index as usize)
            .expect("tried to get upvalue at invalid index")
            .borrow_mut()
    }
}

#[derive(Debug)]
pub struct StringInterner {
    strings: hashbrown::HashSet<InternedString, BuildHasher>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: HashSet::default(),
        }
    }

    pub fn intern(&mut self, s: impl Into<String> + AsRef<str>) -> InternedString {
        let s_ref = Str(s.as_ref());
        match self.strings.get(&s_ref) {
            Some(existing) => existing.clone(),
            None => {
                let new = InternedString::new(s.into());
                self.strings.insert(new.clone());
                new
            }
        }
    }
}
