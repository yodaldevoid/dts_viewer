#![feature(const_fn)]
#![feature(box_syntax)]

#![allow(unused_variables)]
#![allow(dead_code)]

extern crate libc;

mod inner_tree;
mod cpp_parser;
mod flex_bison;

pub use flex_bison::{
	data_free,

	memreserve_add_label,
	node_add_label,
	vec_node_add_label,
	property_add_label,

	build_property,
	build_property_delete,
	chain_property,

	build_node,
	build_node_delete,
	name_node,
	chain_node,
	merge_nodes,
	delete_node,

	build_boot_info,
};

use std::env;
use std::process::Command;
use std::path::{Path, PathBuf};

use inner_tree::*;
use cpp_parser::*;

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

	let mut root_file = ParsedFile::new(&Path::new(&file_path), IncludeMethod::CPP(Vec::new()));

	parse_cpp_outputs(&cpp_stderr, Path::new(CPP_OUTPUT_NAME), &mut root_file);

	println!("{}", root_file);
}
