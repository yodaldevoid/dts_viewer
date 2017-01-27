#![allow(dead_code)]

extern crate libc;
extern crate byteorder;

#[macro_use]
extern crate nom;

mod inner_tree;
mod cpp_parser;
mod dts_parser;

use std::env;
use std::process::Command;
use std::path::{Path, PathBuf};

use cpp_parser::*;
use dts_parser::*;

// Change tracking
/*
struct Change<'a> {
	file: File,
	line: usize,
	// maybe point to node?
	name: String,
	value: Property<'a>,
}
*/

const CPP_OUTPUT_NAME: &'static str = "dts_viewer_tmp.dts";

fn main() {
	let file_name = match env::args().nth(1) {
		None => {
			println!("You forgot the dts file, you dummy");
			return;
		}
		Some(x) => x,
	};

	let arch = "arm";

	let dts_folder = PathBuf::from("arch").join(arch).join("boot/dts/");
	let file_path = dts_folder.join(file_name);

	let include_output = Command::new("arm-linux-gnueabi-gcc")
		.args(&["-H", "-E", "-nostdinc"])
		.args(&["-I", dts_folder.to_str().unwrap()])
		.args(&["-I", dts_folder.join("include/").to_str().unwrap()])
		.args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
		.args(&["-o", CPP_OUTPUT_NAME])
		.arg(&file_path)
		.output()
		.expect("failed to execute process"); //TODO: properly handle errors

	let cpp_stderr = String::from_utf8_lossy(&include_output.stderr);
	println!("{}", cpp_stderr);

	let (root_file, buffer) = match parse_cpp_outputs(&cpp_stderr, Path::new(CPP_OUTPUT_NAME), &file_name) {
		Ok(v) => v,
		Err(e) => {println!("{}", e); return},
	};

	println!("{}", root_file);
}
