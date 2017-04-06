#[macro_use]
extern crate nom;
#[macro_use]
extern crate clap;

mod inner_tree;
mod cpp_parser;
mod dts_parser;
mod change_tracker;

use std::process::Command;
use std::path::{Path, PathBuf};
use std::fs::remove_file;
use std::io::{self, BufRead, Write};
// use std::fs::File;

use cpp_parser::parse_cpp_outputs;
use dts_parser::{Offset, parse_dt};
use change_tracker::LabelStore;

const CPP_OUTPUT_NAME: &'static str = "dts_viewer_tmp.dts";

// General idea:
//  Run CPP
//  Parse file for DTS includes and replace with include contents
//  Find byte starts/ends for each file
//  Parse file to create device tree
//  create mapping of objects to byte offset starting points
//  - not perfect with offsets stored in object, but it will do
//  create mapping of full paths to objects
//  create mapping of labels to full paths
//  Parse device tree to create changes
//  Use mapping and file starts to pair changes to file byte offsets
//  Turn file byte offsets to file lines/cols
//  TODO: ???
//  TODO: Profit!
//  Oh, and somehow display the damn information
fn main() {
    let matches = clap_app!(dts_viewer =>
            (version: crate_version!())
            (author: "Gabriel s. <ga29smith@gmail.com>")
            (@arg file: +required "DTS file to parse")
            (@arg no_defaults: -n --no_defaults "Disable default includes. \
                An 'include' directory, if it exists, is automatically included")
            (@arg include: -I ... +takes_value "Additional files to pass to CPP as an include")
        )
        .get_matches();

    let file_name = matches.value_of("file").unwrap();

    let file = Path::new(file_name);
    let parent = file.parent().expect("Could not get parent directory of file");

    let mut cpp_command = Command::new("gcc");
    cpp_command.args(&["-H", "-E", "-nostdinc"])
        .args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
        .args(&["-o", CPP_OUTPUT_NAME])
        .arg(&file_name);

    if !matches.is_present("no_defaults") {
        cpp_command.args(&["-I", parent.to_str().unwrap()]);
        let include_dir = parent.join("include/");
        if include_dir.is_dir() {
            cpp_command.args(&["-I", include_dir.to_str().unwrap()]);
        }
    }

    if let Some(includes) = matches.values_of("include") {
        for include in includes {
            let include_dir = parent.join(include);
            if include_dir.is_dir() {
                cpp_command.args(&["-I", include_dir.to_str().unwrap()]);
            }
        }
    }

    let include_output = cpp_command.output().expect("Failed to execute CPP");
    let cpp_stderr = String::from_utf8_lossy(&include_output.stderr);
    if !include_output.status.success() {
        println!("Failed to execute CPP. Error message is below.");
        print!("{}", cpp_stderr);
        return;
    }

    let (include_tree, buffer) =
        match parse_cpp_outputs(&cpp_stderr, Path::new(CPP_OUTPUT_NAME), file_name) {
            Ok(v) => v,
            Err(e) => {
                println!("{}", e);
                return;
            }
        };

    if let Err(e) = remove_file(Path::new(CPP_OUTPUT_NAME)) {
        println!("Failed to delete temp file: {:?}", e);
    }

    println!("{:#?}", include_tree);
    // {
    //     let mut total_dts_dump = File::create("total_dts_dump.dts").unwrap();
    //     total_dts_dump.write_all(&buffer).unwrap()
    // }

    let (boot_info, ammends) = match parse_dt(&buffer) {
        Ok(dt) => dt,
        Err(err) => {
            println!("{:?}", err);
            return;
        }
    };

    // TODO: perform secondary checks and jazz (only smooth)
    //
    // checkes to do:
    // duplicate node names
    // duplicate property names
    // node name format (only one @)
    // unit addr vs reg/ranges property
    // duplicate labels -- handled by creation of alias store
    // duplicate explict phandles
    // name property does not match name
    // check if X is cell only
    // #address-cells
    // #size-cells
    // #interrupt-cells
    // check if X is string only
    // device_type
    // model
    // status
    // fixup addr size cells
    // check reg property format
    // check ranges property format
    // avoid default addr size?
    // obsolete chosen iterrupt controller
    //
    // maybe:
    // fixup phandle refs
    // fixup path refs

    let mut store = LabelStore::new();
    store.fill(&boot_info, &ammends);

    loop {
        print!("Enter alias or path: ");
        io::stdout().flush().expect("Error flushing stdout");

        let mut line = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut line).expect("Error reading from stdin");
        let line = line.trim();

        if line.is_empty() {
            break;
        }

        let path = if line.starts_with('/') {
            Some(PathBuf::from(line))
        } else {
            let line = if line.starts_with('&') {
                &line[1..]
            } else {
                line
            };
            // TODO:
            // if label covers multiple paths (after saving old aliases is implemented)
            //     ask user which one they want as well as why this is being asked
            // show all changes along the way
            match store.path_from_label(line) {
                Some(path) => {
                    println!("Path: {}", path.to_string_lossy());
                    Some(path.to_path_buf())
                }
                None => {
                    println!("Label points to no path");
                    None
                }
            }
        };

        if let Some(path) = path {
            match store.changes_from_path(&path) {
                Some(changes) => {
                    for change in changes {
                        let offset = change.get_offset();
                        // println!("Start offset: {}", offset);
                        match include_tree.file_from_offset(offset) {
                            Ok(file) => {
                                print!("File: {}", file.path.to_string_lossy());
                                match include_tree.file_line_from_global(&buffer, offset) {
                                    Ok((line, col)) => {
                                        println!(", Line: {}, Column: {}", line, col)
                                    }
                                    Err(err) => println!("{}", err),
                                }
                            }
                            Err(err) => println!("{}", err),
                        }
                        println!("{}\n", change);
                    }
                }
                None => println!("Nothing at path"),
            }
        }
    }
}
