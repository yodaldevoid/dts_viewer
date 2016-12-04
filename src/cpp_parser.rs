use std::path::{Path, PathBuf};
use std::fs::File;
use std::str::Lines;
use std::io::prelude::*;
use std::io::BufReader;
use std::iter::Peekable;

use inner_tree::*;

#[derive(Debug)]
pub enum LinemarkerFlag {
	Start,
	Return,
	System,
	Extern,
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

pub fn parse_cpp_outputs<'a>(cpp_stderr: &'a str, cpp_output: &Path, root_file: &mut ParsedFile<'a>) {
	parse_cpp_stderr(&mut cpp_stderr.lines().peekable(), root_file, 0);
	parse_cpp_file(cpp_output, root_file);
}

fn parse_cpp_stderr<'a>(lines: &mut Peekable<Lines<'a>>,
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
							IncludeMethod::CPP(Vec::new())
			));
			parse_cpp_stderr(lines,
							 parrent_file.included_files.last_mut().unwrap(),
							 depth + 1);
		} else {
			unreachable!();
		}
	}
}

fn parse_cpp_file<'a>(cpp_output: &Path, root_file: &mut ParsedFile<'a>) {
	let global_buffer = BufReader::new(File::open(cpp_output).unwrap());
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
		parse_cpp_linemarkers(&parsed_lines.next(), &parsed_lines.peek(), Path::new(cpp_output)) {
		if mapping.len > 1 {
			root_file.assign_mapping(path, mapping).unwrap();
		}
	}
}

fn parse_cpp_linemarkers<'a>(current: &'a Option<(usize, usize, PathBuf, Option<LinemarkerFlag>)>,
								next: &Option<&(usize, usize, PathBuf, Option<LinemarkerFlag>)>,
								cpp_output: &Path) -> Option<(&'a Path, FileMapping)> {
	if let Some((c_line_num, c_child_num, ref c_path, _)) = *current {
		if let Some(&(n_line_num, _, _, _)) = *next {
			Some((c_path,
				FileMapping {
					parent_start: c_line_num + 1,
					child_start: c_child_num,
					len: n_line_num - c_line_num,
			}))
		} else {
			let last_line = BufReader::new(File::open(cpp_output).unwrap())
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
