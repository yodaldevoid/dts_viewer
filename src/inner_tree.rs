use std::path::{Path, PathBuf};
use std::fmt::{self, Display, Formatter};
use std::cmp::Ordering;
use std::fs::File;
use std::io::Read;
use std::borrow::Borrow;

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

// TODO: turn into enum with Lines and Bytes forms.
// Use Lines for mapping between CPP files and
// use Bytes for dealing with DTS files,
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

    // TODO: this is a bad idea as a file can be included twice
    pub fn assign_mapping(&mut self, path: &Path, mapping: FileMapping) -> Result<(), String> {
        if self.path == path {
            self.mappings.push(mapping);
            self.mappings.sort();
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

    pub fn file_line_from_global(&self,
                                 global_buffer: &[u8],
                                 global: usize)
                                 -> Result<(usize, usize), String> {
        let file = self.file_from_offset(global)?;
        for mapping in &file.mappings {
            if global >= mapping.parent_start && global < mapping.parent_end() {
                match file.method {
                    IncludeMethod::DTS => {
                        return Ok(byte_offset_to_line_col(
                            File::open(&file.path)
                                .expect(&format!("File not found: {}", file.path.to_str().unwrap()))
                                .bytes()
                                .filter_map(|e| e.ok()),
                            global - mapping.parent_start + mapping.child_start));
                    }
                    IncludeMethod::CPP => {
                        let (g_line, g_col) = byte_offset_to_line_col(global_buffer.iter(), global);
                        let (s_line, s_col) = byte_offset_to_line_col(global_buffer.iter(),
                                                                      mapping.parent_start);
                        let (c_line, c_col) = byte_offset_to_line_col(
                            File::open(&file.path)
                                .expect(&format!("File not found: {}", file.path.to_str().unwrap()))
                                .bytes()
                                .filter_map(|e| e.ok()),
                            mapping.child_start);

                        // println!();
                        // println!("parent_start: {}, child_start: {}",
                        //     mapping.parent_start, mapping.child_start);
                        // println!("g_line: {}, s_line: {}, c_line: {}", g_line, s_line, c_line);
                        // println!("g_col: {}, s_col: {}, c_col: {}", g_col, s_col, c_col);

                        let line = g_line - s_line + c_line;
                        let col = if g_line == s_line {
                            g_col - s_col - c_col
                        } else {
                            g_col - c_col
                        };

                        return Ok((line, col));
                    }
                }
            }
        }

        Err("Failed to find mapping from global offset to file offset".to_string())
    }

    pub fn file_from_offset(&self, offset: usize) -> Result<&ParsedFile, String> {
        if !self.mappings.is_empty() &&
           self.mappings
            .iter()
            .any(|m| offset >= m.parent_start && offset < m.parent_start + m.len) {
            return Ok(self);
        }

        for f in &self.included_files {
            if let Ok(f) = f.file_from_offset(offset) {
                return Ok(f);
            }
        }

        Err(format!("Byte offset is not within parsed files: {}", offset))
    }

    pub fn file_from_offset_mut(&mut self, offset: usize) -> Result<&mut ParsedFile, String> {
        if !self.mappings.is_empty() &&
           self.mappings
            .iter()
            .any(|m| offset >= m.parent_start && offset < m.parent_start + m.len) {
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

    // Takes start of region to split around, end of region,
    // and length to remove due to DTS include statement
    pub fn split_mappings(&mut self, start: usize, end: usize, offset: usize) {
        {
            let mut remainders: Vec<FileMapping> = Vec::new();

            for map in &mut self.mappings {
                if map.parent_start < start && map.parent_end() > start {
                    // parent_start -- start -- parent_end
                    let remainder = map.parent_end() - start;

                    if map.parent_end() > start + offset {
                        // parent_start -- start -- end -- parent_end
                        remainders.push(FileMapping {
                            parent_start: end,
                            child_start: map.child_start + start - map.parent_start + offset,
                            len: remainder - offset,
                        });
                    }

                    map.len = start - map.parent_start;
                } else if map.parent_start == start {
                    // split is at begining of the mapping
                    // offset the start
                    {
                        let offset = end - start;
                        map.parent_start += offset;
                    }
                    // shrink the len by the offset
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
        writeln!(f, "{} {:?}: {}", prefix, self.path, self.method)?;
        // for map in &self.mappings {
        // writeln!(f, "{}, {}, {}", map.parent_start, map.child_start, map.len)?;
        // }
        for file in &self.included_files {
            file.write(f, &next_prefix)?;
        }

        Ok(())
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

pub fn line_to_byte_offset<K, I>(bytes: I, line: usize) -> Result<usize, String>
    where K: Borrow<u8> + Eq,
          I: Iterator<Item = K>
{
    if line == 1 {
        Ok(0)
    } else {
        bytes.enumerate()
            .filter(|&(_, ref byte)| byte.borrow() == &b'\n')
            .nth(line - 2)
            .map(|(offset, _)| offset)
            .ok_or_else(|| "Failed converting from line to byte offset".to_string())
    }
}

pub fn byte_offset_to_line_col<K, I>(bytes: I, offset: usize) -> (usize, usize)
    where K: Borrow<u8> + Eq,
          I: Iterator<Item = K>
{
    let opt = bytes.enumerate()
        .filter(|&(off, ref byte)| off <= offset && byte.borrow() == &b'\n')
        .map(|(start, _)| start)
        .enumerate()
        .last();

    match opt {
        Some((line, start)) => (line + 2, offset - start),
        None => (1, offset + 1),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lines_to_bytes() {
        let string = "Howdy\nHow goes it\nI'm doing fine\n";
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 1).unwrap(),
                   0);
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 2).unwrap(),
                   5);
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 3).unwrap(),
                   17);
    }

    #[test]
    fn bytes_to_lines() {
        let string = "Howdy\nHow goes it\nI'm doing fine\n";
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 0),
                   (1, 1));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 8),
                   (2, 3));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 20),
                   (3, 3));
    }
}
