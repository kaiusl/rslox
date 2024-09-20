use std::fs::read_to_string;
use std::time::{Duration, Instant};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rslox::stack::create_stack;

macro_rules! b {
    ($name:ident) => {
        paste::paste! {
            fn [<bench_ $name>](c: &mut Criterion) {
                let path = format!("./crafting_interpreters_test_files/benchmark.ignore/{}.lox", stringify!($name));
               // println!("{}", path);
                let input =
                    std::fs::read_to_string(path).unwrap();
                bench_core(c, &input, stringify!($name));
            }
        }
    };
}

b!(binary_trees);
b!(fib);
b!(equality);
b!(instantiation);
b!(invocation);
b!(method_call);
b!(properties);
b!(string_equality);
b!(trees);
b!(zoo_batch);
b!(zoo);

fn bench_core(c: &mut Criterion, input: &str, name: &str) {
    c.bench_function(name, move |b| {
        b.iter_custom(|iters| {
            let mut dur = Duration::ZERO;

            for _i in 0..iters {
                let mut out = Vec::<u8>::new();
                let mut outerr = Vec::<u8>::new();
                let mut stack = create_stack();
                let mut vm = rslox::Vm::with_output(&mut stack, &mut out, &mut outerr);
                let input = black_box(input);
                if vm.compile(input).is_ok() {
                    let start = Instant::now();
                    vm.run(input);
                    dur += start.elapsed();
                } else {
                    panic!("failed to compile")
                };
            }

            dur
        })
    });
}

criterion_group!(
    name = benches;
    // This can be any expression that returns a `Criterion` object.
    config = Criterion::default().sample_size(10);
    targets = bench_binary_trees,
              bench_fib,
              bench_equality,
              bench_instantiation,
              bench_invocation,
              bench_method_call,
              bench_properties,
              bench_string_equality,
              bench_trees,
              bench_zoo_batch,
              bench_zoo
);
criterion_main!(benches);
