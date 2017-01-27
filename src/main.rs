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
use std::path::{Path};

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

/*
 *	General idea:
 *		-Run CPP
 *		Parse file for DTS includes and replace with include contents
 *		Find byte starts/ends for each file
 *		Parse file to create device tree and create hashmap of objects to byte offset starting points
 *		Parse device tree to create changes
 *		Use hashmap and file starts to pair changes to file byte offsets
 *		Turn file byte offsets to file lines/rows
 *		???
 *		Profit!
 *		Oh, and somehow display the damn information
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

	//TODO: cmd line option to turn off automatic includes just grab the file
	//TODO: allow passing of additional include directories
	//if default_includes {
		cpp_command.args(&["-I", parent.to_str().unwrap()]);
		let include_dir = parent.join("include/");
		if include_dir.is_dir() {
			cpp_command.args(&["-I", parent.join("include/").to_str().unwrap()]);
		}
	//}

	let include_output = cpp_command.output().expect("failed to execute process"); //TODO: properly handle errors

	let cpp_stderr = String::from_utf8_lossy(&include_output.stderr);
	println!("{}", cpp_stderr);

	let (root_file, buffer) = match parse_cpp_outputs(&cpp_stderr, Path::new(CPP_OUTPUT_NAME), &file_name) {
		Ok(v) => v,
		Err(e) => {println!("{}", e); return},
	};

	println!("{}", root_file);
	println!("{}", String::from_utf8_lossy(&buffer));

	match parse_dt(&buffer) {
		Ok(dt) => println!("{:#?}", dt),
		Err(err) => println!("{:?}", err),
	}

	//TODO: delete CPP output
}
