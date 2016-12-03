use std::env;
use std::process::Command;
use std::path::{Path, PathBuf};
use std::fs::File;
use std::str::Lines;
use std::io::prelude::*;
use std::io::BufReader;
use std::fmt;
use std::iter::Peekable;

// Device Tree stucture
/*
struct DeviceTree<'a> {
	version: u32,
	boot_cpuid: u32,
	reserved_mem: Vec<(u64, u64)>,
	root: Node<'a, 'a>,
}

struct DeviceNode<'a, 'b> {
	name: String,
	props: Vec<(String, Property<'a>)>,
	children: Vec<DeviceNode<'b, 'b>>,
}

enum Property<'a> {
	Empty,
	Cells(Vec<u32>),
	String(String),
	ByteString(Vec<u8>),
	Combo(Vec<&'a Property<'a>>),
}
*/

// File tracking
#[derive(Debug)]
struct ParsedFile<'a> {
	path: &'a Path,
	file: File,
	method: IncludeMethod,
	included_files: Vec<ParsedFile<'a>>,
}

#[derive(Debug)]
enum IncludeMethod {
	DTS,
	CPP(File, Vec<FileMapping>),
}

#[derive(Debug, Clone, Copy)]
struct FileMapping {
	parent_start: usize,
	child_start: usize,
	len: usize,
}

#[derive(Debug)]
enum LinemarkerFlag {
	Start,
	Return,
	System,
	Extern,
}

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

impl<'a> ParsedFile<'a> {
	fn new(path: &'a Path, method: IncludeMethod) -> ParsedFile<'a> {
		ParsedFile {
			path: path,
			file: File::open(path).unwrap(),
			method: method,
			included_files: vec![],
		}
	}

	fn assign_mapping(&mut self, path: &Path, mapping: FileMapping) -> Result<(), String> { //TODO: error propogation
		if self.path == path {
			match self.method {
				IncludeMethod::CPP(_, ref mut mappings) => mappings.push(mapping),
				IncludeMethod::DTS => unreachable!(),
			};
			Ok(())
		} else {
			for file in self.included_files.iter_mut() {
				if file.assign_mapping(path, mapping).is_ok() {
					return Ok(());
				}
			}
			Err(format!("Did not find file: {:?}", path))
		}
	}

	fn write(&self, f: &mut fmt::Formatter, prefix: &str) -> fmt::Result {
		let mut next_prefix = prefix.to_string();
		next_prefix.push_str(" |-");
		writeln!(f, "{} {:?}: {}", prefix, self.path, self.method)
			.and(self.included_files.iter().fold(Ok(()), |res, x| res.and(x.write(f, &next_prefix))))
	}
}

impl<'a> fmt::Display for ParsedFile<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.write(f, "")
	}
}

impl fmt::Display for IncludeMethod {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			IncludeMethod::DTS => write!(f, "DTS"),
			IncludeMethod::CPP(_, _) => write!(f, "CPP"),
		}
	}
}

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

	let cpp_output = String::from_utf8_lossy(&include_output.stderr);
	println!("{}", cpp_output);

	let mut root_file = ParsedFile::new(&Path::new(&file_path),
										IncludeMethod::CPP(File::open(CPP_OUTPUT_NAME).unwrap(), vec![]));

	parse_cpp_output(&mut cpp_output.lines().peekable(), &mut root_file, 0);

	let global_buffer = BufReader::new(File::open(CPP_OUTPUT_NAME).unwrap());
	let mut parsed_lines = global_buffer.lines()
		.enumerate()
		.filter(|&(_, ref line)| match *line {
			Ok(ref l) => l.starts_with('#'),
			Err(_) => false,
		})
		.map(|(line_num, line)| {
			let line = line.unwrap();
			let tokens: Vec<&str> = line.split('"').map(|s| s.trim_matches('#').trim()).collect();
			let child_num: usize = tokens[0].parse().unwrap();
			let flag = match tokens[2].parse().unwrap_or(0) { //TODO: make conversion function
				1 => Some(LinemarkerFlag::Start),
				2 => Some(LinemarkerFlag::Return),
				3 => Some(LinemarkerFlag::System),
				4 => Some(LinemarkerFlag::Extern),
				_ => None,
			};
			(line_num + 1, child_num, PathBuf::from(tokens[1]), flag)
		})
		.peekable();

	while let Some((path, mapping)) =
		parse_cpp_linemarkers(&parsed_lines.next(), &parsed_lines.peek()) {
		if mapping.len > 1 {
			root_file.assign_mapping(path, mapping).unwrap();
		}
	}

	println!("{}", root_file);
}

fn count_begining_chars(s: &str, c: char) -> usize {
	let mut count: usize = 0;
	for sc in s.chars() {
		if sc == c {
			count += 1;
		} else {
			break;
		}
	}

	count
}

fn parse_cpp_output<'a>(lines: &mut Peekable<Lines<'a>>,
						parrent_file: &mut ParsedFile<'a>,
						depth: usize) {
	while let Some(line) = lines.peek().cloned() {
		let count = count_begining_chars(line, '.');

		if count == 0 {
			return;
		} else if count < depth {
			return;
		} else if count == depth {
			return;
		} else if count > depth && (count - depth) == 1 {
			parrent_file.included_files.push(
					ParsedFile::new(
							&Path::new(lines.next()
								.unwrap()
								.trim_left_matches('.')
								.trim_left()),
							IncludeMethod::CPP(File::open(CPP_OUTPUT_NAME).unwrap(), Vec::new())
			));
			parse_cpp_output(lines,
							 parrent_file.included_files.last_mut().unwrap(),
							 depth + 1);
		} else {
			unreachable!();
		}
	}
}

fn parse_cpp_linemarkers<'a>(current: &'a Option<(usize, usize, PathBuf, Option<LinemarkerFlag>)>,
						next: &Option<&(usize, usize, PathBuf, Option<LinemarkerFlag>)>)
						-> Option<(&'a Path, FileMapping)> {
	if let Some((c_line_num, c_child_num, ref c_path, _)) = *current {
		if let Some(&(n_line_num, _, _, _)) = *next {
			Some((c_path,
				FileMapping {
					parent_start: c_line_num + 1,
					child_start: c_child_num,
					len: n_line_num - c_line_num,
			}))
		} else {
			let last_line = BufReader::new(File::open(CPP_OUTPUT_NAME).unwrap())
				.lines()
				.count(); //TODO: don't use lines() as that allocates new Strings on the heap
			Some((c_path,
				FileMapping {
					parent_start: c_line_num + 1,
					child_start: c_child_num,
					len: last_line - c_line_num,
			}))
		}
	} else {
		None
	}
}
