use std::io::Write;
mod common;

fn create_tests(
    prefix: &str,
    root: impl AsRef<str>,
    path: impl AsRef<std::path::Path>,
    out: &mut impl Write,
) {
    let path = path.as_ref();
    let root = root.as_ref();

    let (dirs, files): (Vec<_>, Vec<_>) = path
        .read_dir()
        .unwrap()
        .filter_map(Result::ok)
        .partition(|dir| dir.file_type().unwrap().is_dir());

    for dir in dirs {
        create_tests(prefix, root, dir.path(), out);
    }

    let dir = path.to_str().unwrap();
    if !files.is_empty() && !dir.ends_with(".ignore") {
        writeln!(out, "\n\ntest_dir!(").unwrap();
        writeln!(out, "    {};", prefix).unwrap();
        if dir != root {
            writeln!(
                out,
                "    {};",
                dir.trim_start_matches(root).trim_start_matches('/')
            )
            .unwrap();
        }
        writeln!(out, "    \"{}\";", root).unwrap();

        for file in files {
            if file.path().extension().map(|a| a.to_str().unwrap()) != Some("lox") {
                continue;
            }
            writeln!(
                out,
                "    {},",
                file.path().file_stem().unwrap().to_str().unwrap()
            )
            .unwrap();
        }
        writeln!(out, ");").unwrap();
    }
}

#[test]
#[ignore = "it's used to generate other tests, only run manually"]
fn generate() {
    let mut file = std::fs::File::create("./tests/integration_tests.rs").unwrap();
    writeln!(
        &mut file,
        r##"#![allow(non_snake_case, reason = "it's just a test name and not worth the effort")]"##
    )
    .unwrap();
    writeln!(&mut file, "mod common;").unwrap();
    writeln!(&mut file, "use common::*;").unwrap();

    let root = "./crafting_interpreters_test_files";
    create_tests("base", root, root, &mut file);

    let root = "./test_files";
    create_tests("new", root, root, &mut file);
}
