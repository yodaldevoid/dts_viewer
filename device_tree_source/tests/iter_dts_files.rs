extern crate device_tree_source;
extern crate rayon;
extern crate mktemp;

use std::fs::DirEntry;
use std::process::Command;
use std::path::{PathBuf};

use mktemp::Temp;
use rayon::prelude::*;

use device_tree_source::parser::{ParseResult, parse_dt};
use device_tree_source::include::{IncludeError, include_files};

#[test]
#[ignore]
fn iter_test_dts_files() {
    let path = PathBuf::from("tests/dts");
    let entries: Vec<_> = path.read_dir().expect("tests/dts directory does not exist")
                      .filter_map(|e| e.ok())
                      .filter(|e| e.file_name().to_string_lossy().ends_with(".dts"))
                      .collect();
    let mut oks = Vec::new();
    let mut errs = Vec::new();
    let results: Vec<_> = entries.par_iter().map(try_parse).collect();
    for r in results {
        match r {
            Ok(p) => oks.push(p),
            Err(p) => errs.push(p),
        }
    }

    assert_eq!(errs.len(), 0)
}

fn try_parse(e: &DirEntry) -> Result<PathBuf, PathBuf> {
    let file_name = e.path();
    let cpp_temp_out = Temp::new_file().expect("Could not create temp file");
    let include_output = {
        let parent = file_name.parent().expect("Could not get parent directory of file");

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

        cpp_command.output().expect("Failed to start CPP")
    };

    if !include_output.status.success() {
        println!("Failed to execute CPP. Error message is below.");
        print!("{}", String::from_utf8_lossy(&include_output.stderr));
        return Err(file_name);
    }

    let (buffer, bounds) = match include_files(&cpp_temp_out) {
        Ok(x) => x,
        Err(e) => {
            println!("{}:", file_name.display());
            match e {
                IncludeError::IOError(err, path) => {
                    print!("IO error: {}", err);
                    if let Some(path) = path {
                        println!(" {}", path.display());
                    }
                }
                IncludeError::LinemarkerInDtsi(path) =>
                    println!("Extraneous linemarker found in DT include: {}",
                             path.to_string_lossy()),
                IncludeError::ParseError(_) =>
                    println!("Failed to convert line to byte offset for bounds tracking."),
                IncludeError::NoBoundReturned(path) =>
                    println!("No bounds returned after parsing file: {}", path.to_string_lossy()),
            }
            return Err(file_name);
        }
    };

    // println!("{:#?}", bounds);

    match parse_dt(&buffer) {
        Ok(ParseResult::Complete(..)) => Ok(file_name),
        Ok(ParseResult::RemainingInput(boot_info, amends, rem)) => {
            println!("{}:", file_name.display());
            // println!("Boot Info:\n{:#?}", boot_info);
            // println!("Amends:\n{:#?}", amends);
            // println!("Input remaining after parsing:\n\"{}\"", String::from_utf8_lossy(rem));
            println!("Input remaining after parsing");
            Err(file_name)
        }
        Err(err) => {
            println!("{}: {:?}", file_name.display(), err);
            // println!("{:#?}", bounds);
            // println!("{}", String::from_utf8_lossy(&buffer));
            Err(file_name)
        }
    }
}
