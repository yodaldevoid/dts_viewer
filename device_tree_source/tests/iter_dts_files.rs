#![allow(unused_variables)]

extern crate device_tree_source;
extern crate mktemp;

use std::process::Command;
use std::path::Path;

use mktemp::Temp;

use device_tree_source::parser::{ParseResult, parse_dt};
use device_tree_source::include::{IncludeError, include_files};

macro_rules! generate_tests {
    ($($name:ident $root:expr, $file:expr,)*) => {
        $(
            #[test]
            #[ignore]
            fn $name () {
                $crate::try_parse($root, $file)
            }
        )*
    }
}

include!("dts_file_names");

#[allow(dead_code)]
fn try_parse<P: AsRef<Path>>(root_dir: P, file: P) {
    let root_dir = root_dir.as_ref();
    let file = file.as_ref();
    println!("{}", file.display());

    let mut cpp_temp_out = Temp::new_file().expect("Could not create temp file");
    let mut include_dirs = Vec::new();

    let mut cpp_command = Command::new("gcc");
    cpp_command.args(&["-E", "-nostdinc"])
               .args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
               .args(&["-o", cpp_temp_out.as_ref().to_str().unwrap()])
               .arg(file.to_str().unwrap())
               .args(&["-I", &root_dir.to_string_lossy()]);
    include_dirs.push(root_dir.to_owned());

    if Path::new("include").is_dir() {
        let d = root_dir.join("include/");
        cpp_command.args(&["-I", &d.to_string_lossy()]);
        include_dirs.push(d);
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
