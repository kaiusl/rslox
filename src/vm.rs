use core::panic;
use std::cell::{RefCell, RefMut};
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::{self, Write};
use std::rc::Rc;

use crate::bytecode::{BytesCursor, OpCode};
use crate::common::Span;
use crate::compiler::Compiler;
use crate::value::{
    InternedString, NativeFn, ObjBoundMethod, ObjClass, ObjClosure, ObjInstance, ObjUpvalue,
    Object, Value,
};

use self::error::{RuntimeError, RuntimeErrorKind};

pub mod error;

type Stack<T> = Vec<T>;

#[derive(Debug)]
pub struct Vm<OUT = std::io::Stdout, OUTERR = std::io::Stderr> {
    pub stack: Stack<Value>,
    pub strings: StringInterner,
    pub globals: HashMap<InternedString, Value>,
    pub call_frames: Vec<CallFrame>,
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
    const MAX_FRAMES: usize = 64;
    const STACK_MAX: usize = Self::MAX_FRAMES * (u8::MAX as usize);

    pub fn with_output(output: OUT, outerr: OUTERR) -> Self {
        #[cfg(feature = "debug_trace")]
        let disassembler = Disassembler::new(ByteCode::new(), Vec::new());

        let mut vm = Vm {
            frame: CallFrame::new(),
            stack: Stack::new(),
            strings: StringInterner::new(),
            globals: HashMap::new(),
            call_frames: Vec::new(),
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
        self.stack.push(Value::Nil);
        self.frame.slots = 0;

        Ok(())
    }

    pub fn run(&mut self, input: &str)
    where
        for<'b> &'b mut OUT: io::Write,
        for<'b> &'b mut OUTERR: io::Write,
    {
        if let Err(err) = self.run_core() {
            let report = miette::Report::new(err.to_owned()).with_source_code(input.to_string());
            match writeln!(&mut self.outerr, "{report:?}") {
                Ok(_) => (),
                Err(_) => {
                    eprintln!("failed to report errors from vm, aborting ...");
                    std::process::abort();
                }
            }
        }
    }

    fn run_core(&mut self) -> Result<(), RuntimeError>
    where
        for<'b> &'b mut OUT: io::Write,
    {
        while let Some(op) = self.frame.instructions.try_u8().map(OpCode::from_u8) {
            if self.stack.len() > Self::STACK_MAX {
                let kind = RuntimeErrorKind::Msg("stack overflow".into());
                return Err(self.runtime_error(kind, 1));
            }

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
                OpCode::Return => self.run_op_return(),
                OpCode::Constant => self.run_op_constant(),

                OpCode::DefineGlobal => self.run_op_define_global(),
                OpCode::GetGlobal => self.run_op_get_global()?,
                OpCode::SetGlobal => self.run_op_set_global()?,

                OpCode::GetLocal => self.run_op_get_local(),
                OpCode::SetLocal => self.run_op_set_local(),

                OpCode::GetUpvalue => self.run_on_get_upvalue(),
                OpCode::SetUpvalue => self.run_op_set_upvalue(),

                OpCode::GetProperty => self.run_op_get_property()?,
                OpCode::SetProperty => self.run_op_set_property()?,

                OpCode::Negate => self.run_op_negate()?,
                OpCode::Not => self.run_op_not(),

                OpCode::Add => self.run_binary_add()?,
                OpCode::Subtract => self.binary_arithmetic_op(Self::subtract_number)?,
                OpCode::Multiply => self.binary_arithmetic_op(Self::multiply_number)?,
                OpCode::Divide => self.binary_arithmetic_op(Self::divide_number)?,

                OpCode::Eq => self.run_op_eq(),
                OpCode::Lt => self.binary_cmp_op(Self::lt_number)?,
                OpCode::Gt => self.binary_cmp_op(Self::gt_number)?,

                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),

                OpCode::Print => self.run_op_print(),
                OpCode::Pop => {
                    self.stack.pop();
                }

                OpCode::JumpIfFalse => self.run_op_jump_if_false(),
                OpCode::Jump => self.run_op_jump(),

                OpCode::Loop => self.run_op_loop(),
                OpCode::Call => self.run_op_call()?,
                OpCode::Closure => self.run_op_closure(),
                OpCode::CloseUpvalue => self.run_op_close_upvalue(),

                OpCode::Class => self.run_op_class(),
                OpCode::Method => self.run_op_method(),
                OpCode::Invoke => self.run_op_invoke()?,
                OpCode::Inherit => self.run_op_inherit()?,
                OpCode::GetSuper => self.run_op_get_super()?,
            }
        }

        Ok(())
    }

    fn run_op_return(&mut self) {
        let result = self.stack.pop().expect("expected a return value on stack");
        self.close_upvalues(self.frame.slots);

        self.stack.truncate(self.frame.slots);

        self.stack.push(result);
        self.frame = self
            .call_frames
            .pop()
            .expect("expected a previous call frame to exist when returning from function");
    }

    fn run_op_constant(&mut self) {
        let value = self.frame.expect_constant();
        self.stack.push(value.clone());
    }

    fn run_op_define_global(&mut self) {
        let name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to define global with non string identifier");
        let value = self
            .stack
            .last()
            .expect("tried to define global with no value on stack");
        self.globals.insert(name, value.clone());
        self.stack.pop();
    }

    fn run_op_get_global(&mut self) -> Result<(), RuntimeError> {
        let name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to get global with non string identifier");

        if let Some(value) = self.globals.get(&name) {
            self.stack.push(value.clone());
        } else {
            let kind = RuntimeErrorKind::UndefinedVariable { name };
            return Err(self.runtime_error(kind, 2));
        }

        Ok(())
    }

    fn run_op_set_global(&mut self) -> Result<(), RuntimeError> {
        let name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to set global with non string identifier");

        match self.globals.entry(name) {
            Entry::Occupied(mut entry) => {
                let value = self
                    .stack
                    .last()
                    .expect("tried to set global with no value on the stack")
                    .clone();
                entry.insert(value);
            }
            Entry::Vacant(entry) => {
                let kind = RuntimeErrorKind::UndefinedVariable {
                    name: entry.into_key(),
                };
                return Err(self.runtime_error(kind, 2));
            }
        }
        Ok(())
    }

    fn run_op_get_local(&mut self) {
        let value = self.expect_local().clone();
        self.stack.push(value);
    }

    fn run_op_set_local(&mut self) {
        *self.expect_local() = self
            .stack
            .last()
            .expect("expected value on stack to set local variable")
            .clone();
    }

    fn run_on_get_upvalue(&mut self) {
        let upvalue = self.frame.expect_upvalue();
        match &*upvalue {
            ObjUpvalue::Open(stack_idx) => {
                let value = self
                    .stack
                    .get(*stack_idx)
                    .expect("tried to get upvalue at invalid stack index");
                self.stack.push(value.clone());
            }
            ObjUpvalue::Closed(value) => {
                self.stack.push(value.clone());
            }
        }
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
            .try_to_instance()
        else {
            let kind = RuntimeErrorKind::Msg("only instances have properties".into());
            return Err(self.runtime_error(kind, 1));
        };
        let instance = instance.borrow();
        let prop_name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to get property name from non string");
        let prop_value = instance.properties.get(&prop_name);
        match prop_value {
            Some(value) => {
                self.stack.pop(); // instance
                self.stack.push(value.clone());
            }
            None => {
                self.bind_method(&instance.class.borrow(), prop_name)?;
            }
        }

        Ok(())
    }

    fn run_op_set_property(&mut self) -> Result<(), RuntimeError> {
        // Stack: bottom, .., instance, value_to_set

        let Some(instance) = self
            .stack
            .get(self.stack.len() - 2)
            .expect("expected instance on stack to set property")
            .try_to_instance()
        else {
            let kind = RuntimeErrorKind::Msg("only instances have properties".into());
            return Err(self.runtime_error(kind, 1));
        };
        let name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to set property name from non string");
        let value = self
            .stack
            .pop()
            .expect("expected value on the stack to set property")
            .clone();

        instance.borrow_mut().properties.insert(name, value.clone());
        self.stack.pop(); // instance
        self.stack.push(value);

        Ok(())
    }

    fn run_op_negate(&mut self) -> Result<(), RuntimeError> {
        let value = self
            .stack
            .pop()
            .expect("tried to negate with no value on stack");

        if let Value::Number(v) = value {
            self.stack.push(Value::Number(-v));
        } else {
            let kind = RuntimeErrorKind::InvalidOperand { expected: "number" };
            return Err(self.runtime_error(kind, 1));
        }

        Ok(())
    }

    fn run_op_not(&mut self) {
        let value = self
            .stack
            .pop()
            .expect("tried to not with no value on stack");

        self.stack.push(Value::Bool(value.is_falsey()));
    }

    fn run_op_eq(&mut self) {
        let rhs = self
            .stack
            .pop()
            .expect("tried to eq with no rhs value on stack");
        let lhs = self
            .stack
            .pop()
            .expect("tried to eq with no lhs value on stack");

        self.stack.push(Value::Bool(lhs == rhs));
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

    fn run_op_closure(&mut self) {
        let fun = self
            .frame
            .expect_constant()
            .try_to_function()
            .expect("expected a function on stack for op closure");

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

        let closure = Value::new_closure(ObjClosure::new(fun, upvalues));
        self.stack.push(closure);
    }

    fn run_op_close_upvalue(&mut self) {
        self.close_upvalues(self.stack.len() - 1);
        self.stack.pop();
    }

    fn run_op_class(&mut self) {
        let name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to get class name from non string");

        let class = Value::new_class(ObjClass::new(name));
        self.stack.push(class);
    }

    fn run_op_method(&mut self) {
        // Stack: bottom, .., class, method

        let name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to get method name from non string");
        let class = self
            .stack
            .get(self.stack.len() - 2)
            .expect("expected class to be on stack to define method on")
            .try_to_class()
            .expect("tried to define a method on non class");

        let method = self
            .stack
            .pop()
            .expect("expected value to be on stack to be defined as class method")
            .try_to_closure()
            .expect("expected a closure on stack to be defined as class method");
        class.borrow_mut().methods.insert(name, method);
    }

    fn run_op_invoke(&mut self) -> Result<(), RuntimeError> {
        let name = self
            .frame
            .expect_constant()
            .try_to_string()
            .expect("tried to get method name from non string");
        let arg_count = self
            .frame
            .instructions
            .try_u8()
            .expect("expected a second u8 operand for op invoke");
        self.invoke(name, arg_count)?;
        Ok(())
    }

    fn run_op_inherit(&mut self) -> Result<(), RuntimeError> {
        // Stack: bottom, .., superclass, subclass

        let Some(super_class) = self
            .stack
            .get(self.stack.len() - 2)
            .expect("expected superclass on stack")
            .try_to_class()
        else {
            let kind = RuntimeErrorKind::Msg("superclass must be a class".into());
            return Err(self.runtime_error(kind, 1));
        };
        let class = self
            .stack
            .pop()
            .expect("expected value on stack for op inherit")
            .try_to_class()
            .expect("expected subclass on stack");
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
            .try_to_string()
            .expect("tried to get method name from non string");
        let superclass = self
            .stack
            .pop()
            .expect("expected value on stack for op get super")
            .try_to_class()
            .expect("expected superclass on stack");
        let superclass = superclass.borrow();
        self.bind_method(&superclass, method_name)?;
        Ok(())
    }

    fn invoke(&mut self, name: InternedString, arg_count: u8) -> Result<(), RuntimeError> {
        let Some(instance) =
            self.stack[self.stack.len() - arg_count as usize - 1].try_to_instance()
        else {
            let kind = RuntimeErrorKind::Msg("only instances have methods".into());
            return Err(self.runtime_error(kind, 1));
        };

        let instance = instance.borrow();

        if let Some(field) = instance.properties.get(&name) {
            let idx = self.stack.len() - arg_count as usize - 1;
            self.stack[idx] = field.clone();
            return self.call_value(field.clone(), arg_count);
        }

        let class = instance.class.borrow();
        self.invoke_from_class(&class, name, arg_count)
    }

    fn invoke_from_class(
        &mut self,
        class: &ObjClass,
        name: InternedString,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
        let Some(method) = class.methods.get(&name) else {
            let kind = RuntimeErrorKind::UndefinedProperty { name };
            return Err(self.runtime_error(kind, 1));
        };

        self.call_closure(method, arg_count)
    }

    fn bind_method(&mut self, class: &ObjClass, name: InternedString) -> Result<(), RuntimeError> {
        let Some(method) = class.methods.get(&name) else {
            let kind = RuntimeErrorKind::UndefinedProperty { name };
            return Err(self.runtime_error(kind, 1));
        };

        let receiver = self
            .stack
            .pop()
            .expect("expected receiver on stack")
            .clone();
        let method = ObjBoundMethod::new(receiver, Rc::clone(method));
        let method = Value::new_bound_method(method);

        self.stack.push(method);
        Ok(())
    }

    fn close_upvalues(&mut self, idx: usize) {
        while let Some(last) = self.open_upvalues.last_entry() {
            if *last.key() >= idx {
                *last.get().borrow_mut() = ObjUpvalue::Closed(self.stack[*last.key()].clone());
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
        match callee {
            Value::Object(Object::Closure(fun)) => self.call_closure(&fun, arg_count),
            //Value::Object(Object::Function(fun)) => self.call_function(&fun, arg_count),
            Value::Object(Object::NativeFn(fun)) => {
                let result = fun(
                    arg_count,
                    &self.stack[self.stack.len() - arg_count as usize..],
                );
                self.stack
                    .truncate(self.stack.len() - arg_count as usize - 1);
                self.stack.push(result);

                Ok(())
            }
            Value::Object(Object::Class(cls)) => {
                let instance = ObjInstance::new(Rc::clone(&cls));

                let instance = Value::new_object(Object::Instance(Rc::new(RefCell::new(instance))));
                let receiver_slot = self.stack.len() - arg_count as usize - 1;
                self.stack[receiver_slot] = instance.clone();
                let borrow = cls.borrow();
                if let Some(initializer) = borrow.methods.get(Compiler::INIT_METHOD_NAME) {
                    self.call_closure(initializer, arg_count)?;
                } else if arg_count != 0 {
                    let kind = RuntimeErrorKind::Msg(
                        format!("expected 0 arguments but got {}", arg_count).into(),
                    );
                    return Err(self.runtime_error(kind, 2));
                }

                // self.stack
                //     .truncate(self.stack.len() - arg_count as usize - 1);
                // self.stack.push(instance);
                Ok(())
            }
            Value::Object(Object::BoundMethod(method)) => {
                let receiver_slot = self.stack.len() - arg_count as usize - 1;
                self.stack[receiver_slot] = method.receiver.clone();
                self.call_closure(&method.method, arg_count)
            }

            _ => {
                let kind = RuntimeErrorKind::Msg("can only call functions and classes".into());
                Err(self.runtime_error(kind, 2))
            }
        }
    }

    fn define_native(&mut self, name: impl Into<String> + AsRef<str>, fun: NativeFn) {
        let name = self.strings.intern(name);
        let fun = Object::NativeFn(Rc::new(fun));
        let fun = Value::new_object(fun);
        self.globals.insert(name, fun);
    }

    fn native_clock(_arg_count: u8, _args: &[Value]) -> Value {
        let time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("expected current time to be after unix epoch")
            .as_secs_f64();

        Value::Number(time)
    }

    fn call_closure(
        &mut self,
        closure: &Rc<ObjClosure>,
        arg_count: u8,
    ) -> Result<(), RuntimeError> {
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
            closure: Some(Rc::clone(closure)),
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
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();
        match (lhs, rhs) {
            (Some(Value::Number(lhs)), Some(Value::Number(rhs))) => {
                self.stack.push(Value::Number(op(lhs, rhs)));
            }
            (Some(Value::Object(lhs)), Some(Value::Object(rhs))) => match (lhs, rhs) {
                (Object::String(lhs), Object::String(rhs)) => {
                    let new = lhs.to_string() + &rhs;
                    let new = self.strings.intern(new);
                    self.stack.push(Value::new_object(Object::String(new)));
                }
                _ => {
                    let kind = RuntimeErrorKind::InvalidOperands {
                        expected: "two numbers or string",
                    };
                    return Err(self.runtime_error(kind, 1));
                }
            },
            (Some(lhs), Some(rhs)) => {
                self.stack.push(lhs);
                self.stack.push(rhs);
                let kind = RuntimeErrorKind::InvalidOperands {
                    expected: "two numbers or string",
                };
                return Err(self.runtime_error(kind, 1));
            }
            _ => {
                panic!("tried to do binary add with not enough values on stack")
            }
        }

        Ok(())
    }

    #[inline]
    fn binary_arithmetic_op(&mut self, op: impl Fn(f64, f64) -> f64) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();
        match (lhs, rhs) {
            (Some(Value::Number(lhs)), Some(Value::Number(rhs))) => {
                self.stack.push(Value::Number(op(lhs, rhs)));
            }
            (Some(lhs), Some(rhs)) => {
                self.stack.push(lhs);
                self.stack.push(rhs);
                let kind = RuntimeErrorKind::InvalidOperands { expected: "number" };
                return Err(self.runtime_error(kind, 1));
            }
            _ => {
                panic!("tried to do arithmetic binary op with not enough values on stack")
            }
        }

        Ok(())
    }

    #[inline]
    fn binary_cmp_op(&mut self, op: impl Fn(f64, f64) -> bool) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop();
        let lhs = self.stack.pop();
        match (lhs, rhs) {
            (Some(Value::Number(lhs)), Some(Value::Number(rhs))) => {
                self.stack.push(Value::Bool(op(lhs, rhs)));
            }
            (Some(lhs), Some(rhs)) => {
                self.stack.push(lhs);
                self.stack.push(rhs);
                let kind = RuntimeErrorKind::InvalidOperands { expected: "number" };
                return Err(self.runtime_error(kind, 1));
            }
            _ => {
                panic!("tried to do cmp binary op with not enough values on stack")
            }
        }
        Ok(())
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
    pub spans: Rc<HashMap<usize, Span>>,
    pub constants: Rc<[Value]>,
    pub closure: Option<Rc<ObjClosure>>,
}

impl CallFrame {
    pub fn new() -> Self {
        Self {
            instructions: BytesCursor::new(Vec::new().into()),
            slots: 0,
            spans: Rc::new(HashMap::new()),
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
    strings: HashSet<InternedString>,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: HashSet::new(),
        }
    }

    pub fn intern(&mut self, s: impl Into<String> + AsRef<str>) -> InternedString {
        let s_ref = s.as_ref();
        match self.strings.get(s_ref) {
            Some(existing) => existing.clone(),
            None => {
                let new = InternedString::new(s.into());
                self.strings.insert(new.clone());
                new
            }
        }
    }

    pub fn intern_existing(&mut self, s: InternedString) -> InternedString {
        match self.strings.get(&s) {
            Some(existing) => existing.clone(),
            None => {
                self.strings.insert(s.clone());
                s
            }
        }
    }
}
