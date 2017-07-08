#![allow(unused_variables)]

extern crate device_tree_source;
extern crate walkdir;
extern crate mktemp;
extern crate test;

use std::env::args;
use std::process::Command;
use std::path::{PathBuf, Path};

use walkdir::WalkDir;
use mktemp::Temp;

use test::{test_main, TestDesc, TestDescAndFn, DynTestName, DynTestFn, ShouldPanic};

use device_tree_source::parser::{ParseResult, parse_dt};
use device_tree_source::include::{IncludeError, include_files};

fn main() {
    let args: Vec<_> = args().collect();

    let new_dir = ::std::env::var("DTS_DIR")
                            .map(|env| PathBuf::from(env))
                            .or_else(|_| ::std::env::var("CARGO_MANIFEST_DIR")
                                                  .map(|env| PathBuf::from(env).join("tests/dts")));
    match new_dir {
        Ok(new_dir) => {
            if let Err(_) = ::std::env::set_current_dir(&new_dir) {
                println!("\nCould not change current directory. Does tests/dts exist?\n");
                return
            }
        }
        Err(_) => {
            println!("Both DTS_DIR and CARGO_MANIFEST_DIR unset!");
            return
        }
    }

    let tests = WalkDir::new(".")
                       .follow_links(true)
                       .into_iter()
                       .filter_map(|e| e.ok())
                       .filter(|e| e.file_name().to_string_lossy().ends_with(".dts"))
                       .map(|d| d.path().to_owned())
                       .map(|p| {
                           TestDescAndFn {
                               desc: TestDesc {
                                   name: DynTestName(format!("{}", p.display())),
                                   ignore: true,
                                   should_panic: ShouldPanic::No,
                               },
                               testfn: DynTestFn(Box::new(move || try_parse(&p.clone()))),
                           }
                       })
                       .collect();
    test_main(&args, tests);
}

fn try_parse(file: &PathBuf) {
    let mut cpp_temp_out = Temp::new_file().expect("Could not create temp file");
    let mut include_dirs = Vec::new();

    let mut cpp_command = Command::new("gcc");
    cpp_command.args(&["-E", "-nostdinc"])
               .args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
               .args(&["-o", cpp_temp_out.as_ref().to_str().unwrap()])
               .arg(file.to_str().unwrap())
               .args(&["-I", "."]);
    include_dirs.push(PathBuf::from("."));

    if Path::new("include").is_dir() {
        cpp_command.args(&["-I", "include/"]);
        include_dirs.push(PathBuf::from("include/"));
    }

    if let Some(parent) = file.parent() {
        cpp_command.args(&["-I", &parent.to_string_lossy()]);
        include_dirs.push(parent.to_owned());

        let include = parent.join("include");
        if include.is_dir() {
            cpp_command.args(&["-I", &include.to_string_lossy()]);
            include_dirs.push(include);
        }
    } else {
        panic!("Could not get parent directory of file");
    }

    let include_output = cpp_command.output().expect("Failed to start CPP");

    if !include_output.status.success() {
        // Done to prevent a panic as the file will not have been written to
        cpp_temp_out.release();
        panic!("Failed to execute CPP.\n{:?}\n{}",
               cpp_command,
               String::from_utf8_lossy(&include_output.stderr));
    }

    let (buffer, bounds) = match include_files(&cpp_temp_out, &include_dirs) {
        Ok(x) => x,
        Err(e) => {
            match e {
                IncludeError::IOError(err, path) => {
                    if let Some(path) = path {
                        panic!("IO error: {} {}", err, path.display());
                    } else {
                        panic!("IO error: {}", err);
                    }
                }
                IncludeError::LinemarkerInDtsi(path) =>
                    panic!("Extraneous linemarker found in DT include: {}",
                           path.to_string_lossy()),
                IncludeError::ParseError(_) =>
                    panic!("Failed to convert line to byte offset for bounds tracking."),
                IncludeError::NoBoundReturned(path) =>
                    panic!("No bounds returned after parsing file: {}", path.to_string_lossy()),
            }
        }
    };

    // println!("{:#?}", bounds);

    match parse_dt(&buffer) {
        Ok(ParseResult::Complete(..)) => {},
        Ok(ParseResult::RemainingInput(dt_info, amends, rem)) => {
            // println!("DT Info:\n{:#?}", dt_info);
            // println!("Amends:\n{:#?}", amends);
            panic!("Input remaining after parsing:\n{}", String::from_utf8_lossy(rem));
        }
        Err(err) => {
            // println!("{:#?}", bounds);
            // println!("{}", String::from_utf8_lossy(&buffer));
            panic!("{:?}", err);
        }
    }
}
