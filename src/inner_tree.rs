use std::fs::File;
use std::path::Path;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct ParsedFile<'a> {
	pub path: &'a Path,
	pub file: File,
	pub method: IncludeMethod,
	pub included_files: Vec<ParsedFile<'a>>,
}

#[derive(Debug)]
pub enum IncludeMethod {
	DTS,
	CPP(Vec<FileMapping>),
}

#[derive(Debug, Clone, Copy)]
pub struct FileMapping {
	pub parent_start: usize,
	pub child_start: usize,
	pub len: usize,
}

impl<'a> ParsedFile<'a> {
	pub fn new(path: &'a Path, method: IncludeMethod) -> Self {
		ParsedFile {
			path: path,
			file: File::open(path).unwrap(),
			method: method,
			included_files: vec![],
		}
	}

	pub fn assign_mapping(&mut self, path: &Path, mapping: FileMapping) -> Result<(), String> { //TODO: error propogation
		if self.path == path {
			match self.method {
				IncludeMethod::CPP(ref mut mappings) => mappings.push(mapping),
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

	fn write(&self, f: &mut Formatter, prefix: &str) -> fmt::Result {
		let mut next_prefix = prefix.to_string();
		next_prefix.push_str(" |-");
		writeln!(f, "{} {:?}: {}", prefix, self.path, self.method)
			.and(self.included_files.iter().fold(Ok(()), |res, x| res.and(x.write(f, &next_prefix))))
	}
}

impl<'a> Display for ParsedFile<'a> {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		self.write(f, "")
	}
}

impl Display for IncludeMethod {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match *self {
			IncludeMethod::DTS => write!(f, "DTS"),
			IncludeMethod::CPP(_) => write!(f, "CPP"),
		}
	}
}
