use std::env;
use std::process::Command;
use std::path::Path;
use std::fs::File;
use std::clone::Clone;
use std::str::Lines;
use std::io::prelude::*;
use std::io::BufReader;

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
	method: IncludeMethod<'a>,
	included_files: Vec<ParsedFile<'a>>,
}

#[derive(Debug)]
enum IncludeMethod<'a> {
	DTS,
	CPP(&'a File, Vec<(FileSlice, FileSlice)>),
}

#[derive(Debug)]
struct FileSlice {
	start: usize,
	len: usize,
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
	fn new(path: &'a Path, method: IncludeMethod<'a>) -> ParsedFile<'a> {
		ParsedFile {
			path: path,
			file: File::open(path).unwrap(),
			method: method,
			included_files: vec![],
		}
	}
/*
	fn print(&self, depth: usize) -> String {

	}
*/
}
/*
impl<'a> fmt::Display for ParsedFile<'a> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{:?}: {}", self.file, self.method)
		//Name: method
		//|- Next one
	}
}
*/

fn main() {
	// let file_name = "am335x-boneblack.dts";
	let file_name = match env::args().nth(1) {
		None => {
			println!("You forgot the dts file, you dummy");
			return;
		},
		Some(x) => x,
	};

	// arm-linux-gnueabi-gcc -H -E -nostdinc -Iarch/arm/boot/dts -Iarch/arm/boot/dts/include -undef -D__DTS__ -x assembler-with-cpp -o /dev/null arch/arm/boot/dts/versatile-ab.dts
	let arch = "arm";
	let dts_folder = format!("arch/{}/boot/dts/", arch);
	let file_path = format!("{}{}", dts_folder, file_name);

	let include_output = Command::new("arm-linux-gnueabi-gcc")
								.args(&["-H", "-E", "-nostdinc"])
								.arg(format!("-I{}", dts_folder))
								.arg(format!("-I{}include/", dts_folder))
								.args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
								.args(&["-o", "dts_viewer_tmp.dts"])
								.arg(&file_path)
								.output()
								.expect("failed to execute process"); //TODO: properly handle errors


	let global_file = File::open("dts_viewer_tmp.dts").unwrap();

	let cpp_output = String::from_utf8_lossy(&include_output.stderr);
	println!("{}", cpp_output);

	let mut main_file = ParsedFile::new(
			&Path::new(&file_path),
			IncludeMethod::CPP(&global_file, vec![])
	);

	parse_cpp_output(&mut cpp_output.lines(), &mut main_file, &global_file, 0);

	let global_buffer = BufReader::new(File::open("dts_viewer_tmp.dts").unwrap());
	let mut line_num = 0;
	for line in global_buffer.lines() {
		if let Ok(line) = line {
			line_num += 1;
			if !line.starts_with('#') {
				continue;
			}

			//parse this shit
			let tokens: Vec<&str> = line.split('"').map(|s| s.trim_matches('#').trim()).collect();
/*
			if !main_file.contains(tokens[1]) {
				println!("Not found: {:?}", tokens);
				continue;
			}
*/
			println!("{:?}", tokens);
		} else {
			//set last end as final line num count
		}
	}
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

fn parse_cpp_output<'a>(lines: &mut Lines<'a>, parrent_file: &mut ParsedFile<'a>, global_file: &'a File, depth: usize) {
	while let Some(line) = lines.clone().next() {
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
					&Path::new(lines.next().unwrap().trim_left_matches('.').trim_left()),
					IncludeMethod::CPP(&global_file, vec![])
				)
			);
			parse_cpp_output(lines, parrent_file.included_files.last_mut().unwrap(), global_file, depth + 1);
		} else {
			panic!("Match that should not have been reached.");
		}
	}
}
/*
fn find_included_lines_end(lines: &mut Lines, start_tokens: &[&str], start_line: usize) -> (FileSlice, FileSlice) {

}
*/