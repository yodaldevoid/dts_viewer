#[macro_use]
extern crate nom;

mod inner_tree;
mod cpp_parser;
mod dts_parser;
mod change_tracker;

use std::env;
use std::process::Command;
use std::path::{ Path, PathBuf };
use std::fs::remove_file;
use std::io::{ self, BufRead, Write };

use cpp_parser::parse_cpp_outputs;
use dts_parser::{ Offset, parse_dt };
use change_tracker::LabelStore;

const CPP_OUTPUT_NAME: &'static str = "dts_viewer_tmp.dts";

/*
 * General idea:
 *  Run CPP
 *  Parse file for DTS includes and replace with include contents
 *  Find byte starts/ends for each file
 *  Parse file to create device tree
 *  create mapping of objects to byte offset starting points
    - not perfect with offsets stored in object, but it will do
 *  create mapping of full paths to objects
 *  create mapping of labels to full paths
 *  Parse device tree to create changes
 *  TODO: Use mapping and file starts to pair changes to file byte offsets
 *  Turn file byte offsets to file lines/cols
 *  TODO: ???
 *  TODO: Profit!
 *  TODO: Oh, and somehow display the damn information
 */
fn main() {
    let file_name = match env::args().nth(1) {
        None => {
            println!("You forgot the dts file, you dummy");
            return;
        }
        Some(x) => x,
    };

    let file = Path::new(&file_name);
    let parent = file.parent().unwrap();

    let mut cpp_command = Command::new("arm-linux-gnueabi-gcc");
    cpp_command.args(&["-H", "-E", "-nostdinc"])
        .args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
        .args(&["-o", CPP_OUTPUT_NAME])
        .arg(&file_name);

    // TODO: cmd line option to turn off automatic includes just grab the file
    // TODO: allow passing of additional include directories
    // if default_includes {
    cpp_command.args(&["-I", parent.to_str().unwrap()]);
    let include_dir = parent.join("include/");
    if include_dir.is_dir() {
        cpp_command.args(&["-I", parent.join("include/").to_str().unwrap()]);
    }
    // }

    // TODO: properly handle errors
    let include_output = cpp_command.output().expect("failed to execute process");

    let cpp_stderr = String::from_utf8_lossy(&include_output.stderr);
    println!("{}", cpp_stderr);

    let (root_file, buffer) =
        match parse_cpp_outputs(&cpp_stderr, Path::new(CPP_OUTPUT_NAME), &file_name) {
            Ok(v) => v,
            Err(e) => {
                println!("{}", e);
                return;
            }
        };

    println!("{}", root_file);
    //println!("{}", String::from_utf8_lossy(&buffer));

    match parse_dt(&buffer) {
        Ok(dt) => {
            //println!("{:#?}", dt);
            let (boot_info, ammends) = dt;
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
                    match store.path_from_label(line) {
                        Some(path) => {
                            println!("Path: {:?}", path);
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
                                println!("Start offset: {}", offset);

                                match root_file.file_from_offset(offset) {
                                    Ok(file) => {
                                        print!("Include file: {:?}", file.path);

                                        match root_file.file_line_from_global(&buffer, offset) {
                                            Ok((line, col)) => 
                                                println!(", Line: {}, Column: {}", line, col),
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

            // TODO:
            // if label covers multiple paths (after saving old aliases is implemented)
            //     ask user which one they want as well as why this is being asked
            // show all changes along the way
        },
        Err(err) => println!("{:?}", err),
    }

    // TODO: perform secondary checks and jazz (only smooth)
    /*
        checkes to do: 
            duplicate node names
            duplicate property names
            node name format (only one @)
            unit addr vs reg/ranges property
            duplicate labels -- handled by creation of alias store
            duplicate explict phandles
            name property does not match name
            check if X is cell only
                #address-cells
                #size-cells
                #interrupt-cells
            check if X is string only
                device_type
                model
                status
            fixup addr size cells
            check reg property format
            check ranges property format
            avoid default addr size?
            obsolete chosen iterrupt controller

        maybe:
            fixup phandle refs
            fixup path refs
     */

    match remove_file(Path::new(CPP_OUTPUT_NAME)) {
        Ok(_) => {},
        Err(e) => println!("Failed to delete temp file: {:?}", e),
    }
}
