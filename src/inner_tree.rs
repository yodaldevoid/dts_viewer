use std::path::{Path, PathBuf};
use std::fmt::{self, Display, Formatter};
use std::cmp::Ordering;

#[derive(Debug)]
pub struct ParsedFile {
	pub path: PathBuf,
	pub method: IncludeMethod,
	pub mappings: Vec<FileMapping>,
	pub included_files: Vec<ParsedFile>,
}

#[derive(Debug)]
pub enum IncludeMethod {
	DTS,
	CPP,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FileMapping {
	pub parent_start: usize,
	pub child_start: usize,
	pub len: usize,
}

impl PartialOrd for FileMapping {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for FileMapping {
	fn cmp(&self, other: &Self) -> Ordering {
		self.parent_start.cmp(&other.parent_start)
	}
}

impl FileMapping {
	pub fn parent_end(&self) -> usize {
		self.parent_start + self.len
	}

	pub fn child_end(&self) -> usize {
		self.parent_start + self.len
	}
}

impl ParsedFile {
	pub fn new(path: PathBuf, method: IncludeMethod) -> Self {
		ParsedFile {
			path: path,
			method: method,
			mappings: vec![],
			included_files: vec![],
		}
	}

	//TODO: this is a bad idea as a file can be included twice
	pub fn assign_mapping(&mut self, path: &Path, mapping: FileMapping) -> Result<(), String> {
		if self.path == path {
			self.mappings.push(mapping);
			self.mappings.sort();
			//TODO: sanity checks?
			Ok(())
		} else {
			for file in &mut self.included_files {
				if file.assign_mapping(path, mapping).is_ok() {
					return Ok(());
				}
			}
			Err(format!("Did not find file: {:?}", path))
		}
	}

	pub fn add_include(&mut self, other: ParsedFile) {
		self.included_files.push(other);
		if self.included_files.iter().all(|f| f.mappings.len() >= 1) {
			self.included_files.sort_by_key(|f| f.mappings[0]);
		}
		//TODO: sanity checks?
	}

	pub fn bounds_of_tree(&self) -> Result<(usize, usize), String> {
		let first_map = self.mappings.first().ok_or("No mappings")?;

		let mut start = first_map.parent_start;
		let mut end = first_map.parent_end();

		if let Some(last_map) = self.mappings.last() {
			let e = last_map.parent_end();
			if e > end {
				end = e;
			}
		}

		if let Some(file) = self.included_files.first() {
			let (s, e) = file.bounds_of_tree()?;
			if s < start {
				start = s;
			}
			if e > end {
				end = e;
			}
		}

		if let Some(file) = self.included_files.last() {
			let (_, e) = file.bounds_of_tree()?;
			if e > end {
				end = e;
			}
		}

		Ok((start, end))
	}

	pub fn file_from_offset_mut(&mut self, offset: usize) -> Result<&mut ParsedFile, String> {
		if !self.mappings.is_empty() && self.mappings.iter().any(|m| offset >= m.parent_start && offset < m.parent_start + m.len) {
			return Ok(self);
		}

		for f in &mut self.included_files {
			if let Ok(f) = f.file_from_offset_mut(offset) {
				return Ok(f);
			}
		}

		Err(format!("Byte offset is not within parsed files: {}", offset))
	}

	pub fn offset_after_location(&mut self, start: usize, offset: isize) {
		for map in &mut self.mappings {
			if map.parent_start > start {
				if offset.is_positive() {
					map.parent_start += offset as usize;
				} else {
					map.parent_start -= offset.abs() as usize;
				}
			}
		}

		for file in &mut self.included_files {
			file.offset_after_location(start, offset);
		}
	}

	// Takes start of region to split around, end of region, and length to remove due to DTS include statement
	pub fn split_mappings(&mut self, start: usize, end: usize, offset: usize) {
		{
			let mut remainders: Vec<FileMapping> = Vec::new();

			for map in &mut self.mappings {
				if map.parent_start < start && map.parent_end() > start  {
					// parent_start -- start -- parent_end
					let remainder = map.parent_end() - start;

					if map.parent_end() > start + offset {
						// parent_start -- start -- end -- parent_end
						remainders.push(FileMapping {
							parent_start: end,
							child_start: map.child_start + start - map.parent_start + offset, //TODO: this aint right
							len: remainder - offset,
						});
					} //TODO: check for split across two includes

					map.len = start - map.parent_start;
				} else if map.parent_start == start {
					//split is at begining of the mapping
					//offset the start
					{
						let offset = end as isize - start as isize;
						if offset.is_positive() {
							map.parent_start += offset as usize;
						} else {
							map.parent_start -= offset.abs() as usize;
						}
					}
					//shrink the len by the offset
					map.len -= offset;
				}
			}

			self.mappings.extend_from_slice(&remainders);
			self.mappings.sort();
		}

		for file in &mut self.included_files {
			file.split_mappings(start, end, offset);
		}
	}

	fn write(&self, f: &mut Formatter, prefix: &str) -> fmt::Result {
		let mut next_prefix = prefix.to_string();
		next_prefix.push_str(" |-");
		writeln!(f, "{} {:?}: {}", prefix, self.path, self.method)
			.and(self.included_files.iter().fold(Ok(()), |res, x| res.and(x.write(f, &next_prefix))))
	}
}

impl Display for ParsedFile {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		self.write(f, "")
	}
}

impl Display for IncludeMethod {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match *self {
			IncludeMethod::DTS => write!(f, "DTS"),
			IncludeMethod::CPP => write!(f, "CPP"),
		}
	}
}
