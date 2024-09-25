#![allow(
    dead_code,
    unused_macros,
    unused_imports,
    reason = "these methods are used in other modules of integration tests, but compiler doesn't see it"
)]

pub(crate) fn test_file(prefix: &str, root: impl AsRef<str>, path: impl AsRef<std::path::Path>) {
    let root = root.as_ref();
    miette::set_hook(Box::new(|_| {
        Box::new(
            miette::MietteHandlerOpts::new()
                .color(false)
                .wrap_lines(false)
                .break_words(false)
                .build(),
        )
    }))
    .ok();

    let path = path.as_ref();
    let src = std::fs::read_to_string(path).unwrap();
    let mut out = Vec::<u8>::new();
    let mut outerr = Vec::<u8>::new();
    let mut stack = create_stack();
    let mut vm = rslox::Vm::with_output(&mut stack, &mut out, &mut outerr);
    if vm.compile(&src).is_ok() {
        vm.run(&src);
    };

    let root_dir = &root[2..];
    let mut settings = insta::Settings::clone_current();
    let mut suffix = String::from(prefix);
    for c in path.components() {
        let str = c.as_os_str().to_str().unwrap();
        if !str.is_empty() && str != "." && str != root_dir {
            if !suffix.is_empty() {
                suffix.push('@');
            }
            suffix.push_str(c.as_os_str().to_str().unwrap());
        }
    }

    #[cfg(not(miri))]
    {
        settings.set_snapshot_suffix(suffix);
        settings.set_input_file(path);
        settings.set_description(src);
        settings.set_prepend_module_to_snapshot(false);
        settings.set_omit_expression(true);
        settings.set_snapshot_path("../snapshots");
        let _quard = settings.bind_to_scope();
        insta::assert_snapshot!(format!(
            "OUT: \n{}\n===\nERR: \n{}",
            String::from_utf8(out).unwrap(),
            String::from_utf8(outerr).unwrap()
        ))
    }
}

// Note that we append _ to the test name so that we can use Rust keywords as $file
macro_rules! test_dir {
    ($prefix:ident; $type:tt; $root:literal; $($file:tt),+ $(,)?) => {
        ::paste::paste!{
            #[::rstest::rstest]
            $(
                #[case::[<$file _>](concat!(stringify!($file), ".lox"))]
            )+
            fn [<test_ $prefix _ $type>](#[case] path: &str) {
                test_file(::std::stringify!($prefix), $root, ::std::format!("{}/{}/{}", $root, ::std::stringify!($type), path));
            }
        }
    };
    ($prefix:ident; $root:literal; $($file:tt),+ $(,)?) => {
        ::paste::paste!{
            #[::rstest::rstest]
            $(
                #[case::[<$file _>](concat!(stringify!($file), ".lox"))]
            )+
            fn [< test_ $prefix  _root>](#[case] path: &str) {
                test_file(::std::stringify!($prefix), $root, ::std::format!("{}/{}", $root,  path));
            }
        }
    };
}

use rslox::stack::create_stack;
pub(crate) use test_dir;
