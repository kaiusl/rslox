use std::io::Write;
mod common;

use common::ROOT;

fn create_tests(path: impl AsRef<std::path::Path>, out: &mut impl Write) {
    let path = path.as_ref();

    let (dirs, files): (Vec<_>, Vec<_>) = path
        .read_dir()
        .unwrap()
        .filter_map(Result::ok)
        .partition(|dir| dir.file_type().unwrap().is_dir());

    for dir in dirs {
        create_tests(dir.path(), out);
    }

    let dir = path.to_str().unwrap();
    if !files.is_empty() && !dir.ends_with(".ignore") {
        writeln!(out, "\n\ntest_dir!(").unwrap();
        if dir != ROOT {
            writeln!(
                out,
                "    {};",
                dir.trim_start_matches(ROOT).trim_start_matches('/')
            )
            .unwrap();
        }
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

    create_tests(ROOT, &mut file);
}
