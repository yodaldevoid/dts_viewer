#![allow(unused_variables)]

extern crate device_tree_source;
extern crate mktemp;
extern crate test;

use std::env::args;
use std::process::Command;
use std::path::{PathBuf, Path};

use mktemp::Temp;

use test::{test_main, TestDesc, TestDescAndFn, DynTestName, DynTestFn, ShouldPanic};

use device_tree_source::parser::{ParseResult, parse_dt};
use device_tree_source::include::{IncludeError, include_files};

fn main() {
    let args: Vec<_> = args().collect();

    let tests = Path::new("tests/dts")
                    .read_dir().expect("tests/dts directory does not exist")
                    .filter_map(|e| e.ok())
                    .filter(|e| e.file_name().to_string_lossy().ends_with(".dts"))
                    .map(|d| d.path())
                    .map(|p| {
                        TestDescAndFn {
                            desc: TestDesc {
                                name: DynTestName(format!("{}", p.display())),
                                ignore: true,
                                should_panic: ShouldPanic::No,
                            },
                            testfn: DynTestFn(Box::new(move || try_parse(p.clone()))),
                        }
                    })
                    .collect();
    test_main(&args, tests);
}

fn try_parse(file_name: PathBuf) {
    let mut cpp_temp_out = Temp::new_file().expect("\nCould not create temp file");
    let include_output = {
        let parent = file_name.parent().expect("\nCould not get parent directory of file");

        let mut cpp_command = Command::new("gcc");
        cpp_command.args(&["-E", "-nostdinc"])
                   .args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
                   .args(&["-o", cpp_temp_out.as_ref().to_str().unwrap()])
                   .arg(file_name.to_str().unwrap())
                   .args(&["-I", parent.to_str().unwrap()]);

        let include_dir = parent.join("include/");
        if include_dir.is_dir() {
            cpp_command.args(&["-I", include_dir.to_str().unwrap()]);
        }

        // println!("{:?}", cpp_command);

        cpp_command.output().expect("\nFailed to start CPP")
    };

    if !include_output.status.success() {
        // Done to prevent a panic as the file will not have been written to
        cpp_temp_out.release();
        panic!("\nFailed to execute CPP. Error message is below.\n{}",
               String::from_utf8_lossy(&include_output.stderr));
    }

    let (buffer, bounds) = match include_files(&cpp_temp_out) {
        Ok(x) => x,
        Err(e) => {
            match e {
                IncludeError::IOError(err, path) => {
                    if let Some(path) = path {
                        panic!("\nIO error: {} {}", err, path.display());
                    } else {
                        panic!("\nIO error: {}", err);
                    }
                }
                IncludeError::LinemarkerInDtsi(path) =>
                    panic!("\nExtraneous linemarker found in DT include: {}",
                           path.to_string_lossy()),
                IncludeError::ParseError(_) =>
                    panic!("\nFailed to convert line to byte offset for bounds tracking."),
                IncludeError::NoBoundReturned(path) =>
                    panic!("\nNo bounds returned after parsing file: {}", path.to_string_lossy()),
            }
        }
    };

    // println!("{:#?}", bounds);

    match parse_dt(&buffer) {
        Ok(ParseResult::Complete(..)) => {},
        Ok(ParseResult::RemainingInput(boot_info, amends, rem)) => {
            // println!("Boot Info:\n{:#?}", boot_info);
            // println!("Amends:\n{:#?}", amends);
            panic!("\nInput remaining after parsing:\n{}", String::from_utf8_lossy(rem));
        }
        Err(err) => {
            // println!("{:#?}", bounds);
            // println!("{}", String::from_utf8_lossy(&buffer));
            panic!("\n{:?}", err);
        }
    }
}
