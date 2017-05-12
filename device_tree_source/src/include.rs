//! This module includes functions and structures that deal with the parsing and
//! execution of Device Tree `/include/` statements as well as the mapping from
//! the global buffer returned by `include_files` back to the original file.

use std::str::{self, FromStr};
use std::path::{PathBuf, Path};
use std::fs::File;
use std::io::{self, Read};
use std::cmp::Ordering;

use nom::{IResult, ErrorKind, Needed, FindSubstring, digit, space, multispace, line_ending};

use parser::escape_c_string;
use ::{byte_offset_to_line_col, line_to_byte_offset};

/// Defines errors from manipulating IncludeBounds.
// TODO: impl Display and Error - issue 1.2
#[derive(Debug)]
pub enum BoundsError {
    /// The given offset was not within the collection of bounds or single
    /// `IncludeBounds`.
    NotWithinBounds,
    /// Some IO Error. Probably from trying to open a file. May include the path
    /// trying to be opened.
    IOError(io::Error, Option<PathBuf>),
    /// Some `ParseError`. Probably from a failed attempt to convert from lines
    /// to byte offsets.
    ParseError(::ParseError)
}

impl From<io::Error> for BoundsError {
    fn from(err: io::Error) -> Self {
        BoundsError::IOError(err, None)
    }
}

impl From<::ParseError> for BoundsError {
    fn from(err: ::ParseError) -> Self {
        BoundsError::ParseError(err)
    }
}

/// Defines various errors that may happen in the include process.
// TODO: impl Display and Error - issue 1.3
#[derive(Debug)]
pub enum IncludeError {
    /// No bounds returned after parsing file
    NoBoundReturned(PathBuf),
    /// Extraneous CPP linemarker found in file included by DT `/include/`
    /// statement. This **should** never happen, but if it does the file where
    /// the linemarker was found needs to be cleaned up.
    LinemarkerInDtsi(PathBuf),
    /// Some IO Error. Probably from trying to open a file. May include the path
    /// trying to be opened.
    IOError(io::Error, Option<PathBuf>),
    /// Some `ParseError`. Probably from a failed attempt to convert from lines
    /// to byte offsets.
    ParseError(::ParseError)
}

impl From<io::Error> for IncludeError {
    fn from(err: io::Error) -> Self {
        IncludeError::IOError(err, None)
    }
}

impl From<::ParseError> for IncludeError {
    fn from(err: ::ParseError) -> Self {
        IncludeError::ParseError(err)
    }
}

/// Stores the information to map a section of the buffer returned by
/// `include_files` to the original file. Often does not map a whole file, but
/// only a part starting at at `child_start`. The mapped section starts at
/// `start()` bytes in the global buffer and continues for `len()` bytes.
/// `len()` does not indicate the length in bytes that the `IncludeBounds` maps
/// to in the original file as if the file has been processed by the C
/// preprocessor whitespace may have been removed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludeBounds {
    path: PathBuf,
    global_start: usize,
    child_start: usize,
    len: usize,
    method: IncludeMethod,
}

/// Specifies the method used to include a file.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncludeMethod {
    /// File was included using the device tree specification's `/include/`
    /// statement.
    DTS,
    /// File was included using the C preprocessor.
    CPP,
}

impl PartialOrd for IncludeBounds {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for IncludeBounds {
    fn cmp(&self, other: &Self) -> Ordering {
        use std::cmp::Ordering::*;
        match self.start().cmp(&other.start()) {
            Equal => self.end().cmp(&other.end()),
            o => o,
        }
    }
}

impl IncludeBounds {
    /// Returns the path to the file which this bound maps to.
    pub fn child_path(&self) -> &Path {
        &self.path
    }

    /// Returns the start of the bound in the global buffer, in bytes.
    ///
    /// This is inclusive and as such the byte at the returned offset is
    /// part of this bound.
    pub fn start(&self) -> usize {
        self.global_start
    }

    /// Returns the end of the bound in the global buffer, in bytes.
    ///
    /// This is non-inclusive and as such the byte at the returned offset
    /// is not part of this bound.
    pub fn end(&self) -> usize {
        self.global_start + self.len
    }

    /// The total length in bytes of the bound.
    pub fn len(&self) -> usize {
        self.len
    }

    /// The start of the bound in the file this bound maps to, in bytes.
    ///
    /// Simply offsetting from this position within the file does
    /// not always give the intended position as the C preprocessor can, and
    /// will, remove whitespace that is in the original file.
    /// Use `file_line_from_global` to retrieve the real position within a file
    /// for a given offset.
    pub fn child_start(&self) -> usize {
        self.child_start
    }

    /// Returns the method that was used to include the file that this bound
    /// bound maps to.
    pub fn include_method(&self) -> &IncludeMethod {
        &self.method
    }

    fn split_bounds(bounds: &mut Vec<IncludeBounds>, start: usize, end: usize, offset: usize) {
        let mut remainders: Vec<IncludeBounds> = Vec::new();

        // println!("split s: {} e: {} off: {}", start, end, offset);

        for b in bounds.iter_mut() {
            // println!("b.start: {} b.end: {} b.len: {}", b.start(), b.end(), b.len());
            if b.start() <= start && b.end() >= start {
                // global_start -- start -- global_end
                let remainder = b.end() - start;

                // println!("remainder: {}", remainder);

                remainders.push(IncludeBounds {
                    path: b.path.clone(),
                    global_start: end,
                    child_start: b.start() + start - b.start() + offset,
                    len: remainder, // - offset,
                    method: b.include_method().clone(),
                });

                b.len = start - b.start();
            }
        }

        bounds.extend_from_slice(&remainders);
        bounds.sort();
    }

    /// Find the line and column of a file given an offset into the global
    /// buffer.
    ///
    /// # Errors
    /// Returns `NotInBounds` if the offset given is not within the
    /// bounds specified by this IncludeBound.
    /// Returns `ParseError` on failure to convert offset to line
    /// and column.
    /// Returns `IOError` on failure to open a file.
    pub fn file_line_from_global(&self,
                                 global_buffer: &[u8],
                                 offset: usize)
                                 -> Result<(usize, usize), BoundsError> {
        if offset >= self.global_start && offset < self.end() {
            match self.method {
                IncludeMethod::DTS => {
                    let b = match File::open(&self.path) {
                            Ok(f) => f,
                            Err(e) => return Err(BoundsError::IOError(e, Some(self.path.to_owned()))),
                        }
                        .bytes().filter_map(|e| e.ok());
                    byte_offset_to_line_col(b, offset - self.global_start + self.child_start)
                                            .map_err(|e| e.into())
                }
                IncludeMethod::CPP => {
                    let (g_line, g_col) = byte_offset_to_line_col(global_buffer.iter(), offset)?;
                    let (s_line, s_col) = byte_offset_to_line_col(global_buffer.iter(),
                                                                  self.global_start)?;
                    let b = match File::open(&self.path) {
                            Ok(f) => f,
                            Err(e) => return Err(BoundsError::IOError(e, Some(self.path.to_owned()))),
                        }
                        .bytes().filter_map(|e| e.ok());
                    let (c_line, c_col) = byte_offset_to_line_col(b, self.child_start)?;

                    // println!();
                    // println!("global_start: {}, child_start: {}",
                    //          self.global_start, self.child_start);
                    // println!("g_line: {}, s_line: {}, c_line: {}", g_line, s_line, c_line);
                    // println!("g_col: {}, s_col: {}, c_col: {}", g_col, s_col, c_col);

                    let line = g_line - s_line + c_line;
                    //TODO: find more rigorous way of testing this
                    let col = if g_line == s_line {
                        g_col - s_col - c_col + 2
                    } else {
                        g_col - c_col + 1
                    };

                    Ok((line, col))
                }
            }
        } else {
            Err(BoundsError::NotWithinBounds)
        }
    }
}

/// Performs a binary search on the collection of bounds and returns the one containing the offset.
///
/// # Errors
/// Returns `NotWithinBounds` if the `IncludeBounds` containing the offset cannot be found.
pub fn get_bounds_containing_offset<'a>(bounds: &'a [IncludeBounds],
                                        offset: usize)
                                        -> Result<&'a IncludeBounds, BoundsError> {
    match bounds.binary_search_by(|b| {
        use std::cmp::Ordering::*;
        match (b.start().cmp(&offset), b.end().cmp(&offset)) {
            (Less, Greater) | (Equal, Greater) => Equal,
            (Greater, Greater) => Greater,
            (Equal, Less) | (Less, Less) | (Less, Equal) | (Equal, Equal) => Less,
            _ => unreachable!(),
        }
    }) {
        Ok(off) => Ok(&bounds[off]),
        Err(_) => Err(BoundsError::NotWithinBounds),
    }
}

#[derive(Debug, PartialEq)]
struct Linemarker {
    child_line: usize,
    path: PathBuf,
    flag: Option<LinemarkerFlag>,
}

#[derive(Debug, PartialEq)]
enum LinemarkerFlag {
    Start,
    Return,
    System,
    Extern,
}

named!(parse_linemarker<Linemarker>,
    complete!(do_parse!(
        tag!("#") >>
        opt!(tag!("line")) >>
        space >>
        line: map_res!(map_res!(digit, str::from_utf8), usize::from_str) >>
        space >>
        path: delimited!(
            char!('"'),
            map!(escape_c_string, PathBuf::from),
            char!('"')
        ) >>
        flag: opt!(preceded!(space, map_res!(map_res!(digit, str::from_utf8), u64::from_str))) >>
        line_ending >>
        (Linemarker {
            child_line: line,
            path: path,
            flag: flag.map(|f| match f {
                1 => LinemarkerFlag::Start,
                2 => LinemarkerFlag::Return,
                3 => LinemarkerFlag::System,
                4 => LinemarkerFlag::Extern,
                _ => unreachable!(),
            }),
        })
    ))
);

fn find_linemarker_start(input: &[u8]) -> IResult<&[u8], &[u8]> {
    if "# ".len() > input.len() {
        IResult::Incomplete(Needed::Size("# ".len()))
    } else {
        match input.find_substring("# ").iter().chain(input.find_substring("#line ").iter()).min() {
            None => {
                IResult::Error(error_position!(ErrorKind::TakeUntil, input))
            },
            Some(index) => {
                IResult::Done(&input[*index..], &input[0..*index])
            },
        }
    }
}

named!(find_linemarker<(&[u8], Linemarker)>, do_parse!(
    pre: find_linemarker_start >>
    marker: parse_linemarker >>
    (pre, marker)
));

fn parse_linemarkers(buf: &[u8],
                     bounds: &mut Vec<IncludeBounds>,
                     global_offset: usize,
                     post_len: usize)
                     -> Result<(), IncludeError> {
    let end_offset = global_offset + buf.len();
    // println!("{}", String::from_utf8_lossy(buf));
    let mut buf = buf;
    // println!("parsing linemarkers");
    while let IResult::Done(rem, (pre, marker)) = find_linemarker(buf) {
        // println!("{:?}", marker);
        // println!("pre.len() {}", pre.len());
        // println!("rem.len() {}", rem.len());
        // println!("post_len {}", post_len);

        // println!("{:#?}", bounds);
        // double check that last bound was from a linemarker
        match bounds.last_mut() {
            Some(ref mut bound) if bound.method == IncludeMethod::CPP => { bound.len = pre.len() }
            Some(&mut IncludeBounds{ ref path, .. }) =>
                return Err(IncludeError::LinemarkerInDtsi(path.to_owned())),
            None => unreachable!(),
        }

        // start at new line
        let new_bound = IncludeBounds {
            path: marker.path.clone(),
            global_start: end_offset - rem.len(),
            child_start: match File::open(&marker.path) {
                Ok(f) => line_to_byte_offset(f.bytes().filter_map(|e| e.ok()), marker.child_line)?,
                Err(_) => 0,
            },
            len: rem.len() + post_len,
            method: IncludeMethod::CPP,
        };

        bounds.push(new_bound);

        buf = rem;
    }

    Ok(())
}

named!(parse_include<String>, complete!(preceded!(
    tag!("/include/"),
    preceded!( multispace,
        delimited!(
            char!('"'),
            escape_c_string,
            char!('"')
        ))
)));

fn find_include<'a>(buf: &'a [u8]) -> Option<(&'a [u8], PathBuf, &'a [u8])> {
    for (index, win) in buf.windows("/include/".len()).enumerate() {
        if win == b"/include/" {
            match parse_include(&buf[index..]) {
                IResult::Done(rem, file) => {
                    return Some((&buf[..index], PathBuf::from(file), rem))
                }
                IResult::Error(_) => {}
                IResult::Incomplete(_) => unreachable!(),
            }
        }
    }

    return None
}

/// Parses `/include/` statements in the file returning a buffer with all files
/// included.
///
/// Also returns the bounds of each included file within the buffer to allow
/// mapping from the buffer to the original files. See
/// `IncludeBounds::file_line_from_global` for more information. The
/// `IncludeBounds` can be ignored if mapping from the final buffer to the
/// original file is not needed.
/// 
/// If C style `#include` statements need to be parsed that step should be
/// performed, usually by running CPP, before calling this function on the file
/// output from that step.
///
/// `include_dirs` should be a list of all directories to search for included
/// files. No directories are assumed, including the current directory. If no
/// directories are  passed in, the function will fail.
///
/// # Errors
/// Returns `IOError` if any file cannot be opened.
/// Returns `ParseError` if any line is unable to be converted to
/// an offset.
/// Returns `NoBoundReturned` if something really went wrong
/// while parsing a included file.
/// Returns `LinemarkerInDtsi` if a C preprocessor linemarker is found within a
/// file included by an `/include/` statement. This should never happen, and if
/// it does that file needs to be cleaned up.
pub fn include_files<P: AsRef<Path>, I: AsRef<Path>>(file: P, include_dirs: &[I]) -> Result<(Vec<u8>, Vec<IncludeBounds>), IncludeError> {
    fn _include_files(path: &Path,
                      include_dirs: &[&Path],
                      main_offset: usize)
                      -> Result<(Vec<u8>, Vec<IncludeBounds>), IncludeError> {
        fn find_file(path: &Path, include_dirs: &[&Path]) -> Result<PathBuf, IncludeError> {
            for dir in include_dirs.iter() {
                let p = dir.join(path);
                if p.is_file() {
                    return Ok(p);
                }
            }
            Err(IncludeError::IOError(io::Error::from(io::ErrorKind::NotFound), Some(path.to_owned())))
        }
        let path = find_file(path, include_dirs)?;
        let mut file = match File::open(&path) {
            Ok(f) => f,
            Err(e) => return Err(IncludeError::IOError(e, Some(path.to_owned()))),
        };
        let mut buffer: Vec<u8> = Vec::new();
        let mut bounds: Vec<IncludeBounds> = Vec::new();

        let mut string_buffer = String::new();
        file.read_to_string(&mut string_buffer)?;

        // println!("{}", string_buffer);
        // println!();

        let mut buf = string_buffer.as_bytes();

        named!(first_linemarker<(&[u8], Linemarker)>,
            do_parse!(
                marker: peek!(parse_linemarker) >>
                line: recognize!(parse_linemarker) >>
                (line, marker)
            )
        );

        let start_bound = if let IResult::Done(rem, (line, marker)) = first_linemarker(buf) {
            let bound = IncludeBounds {
                path: marker.path.clone(),
                global_start: buf.len() - rem.len(),
                child_start: {
                    let b = match File::open(&marker.path) {
                            Ok(f) => f,
                            Err(e) => return Err(IncludeError::IOError(e, Some(marker.path.to_owned()))),
                        }
                        .bytes().filter_map(|e| e.ok());
                    line_to_byte_offset(b, marker.child_line)?
                },
                len: match File::open(&marker.path) {
                        Ok(f) => f,
                        Err(e) => return Err(IncludeError::IOError(e, Some(marker.path.to_owned()))),
                    }
                    .bytes().count(),
                method: IncludeMethod::CPP,
            };

            buffer.extend_from_slice(line);
            buf = rem;

            bound
        } else {
            // println!("main_offset {}", main_offset);
            IncludeBounds {
                path: path.to_owned(),
                global_start: main_offset,
                child_start: 0,
                len: match File::open(&path) {
                        Ok(f) => f,
                        Err(e) => return Err(IncludeError::IOError(e, Some(path.to_owned()))),
                    }
                    .bytes().count(),
                method: IncludeMethod::DTS,
            }
        };
        bounds.push(start_bound);

        while let Some((pre, included_path, rem)) = find_include(&buf[..]) {
            parse_linemarkers(pre, &mut bounds, buffer.len(), buf.len() - pre.len())?;
            buffer.extend_from_slice(pre);

            let offset = pre.len();
            // println!("{}", file.display());
            // println!("Offset: {}", offset);
            // println!("{}", bounds);

            let total_len = buffer.len() + main_offset; // - 1;
            let (sub_buf, sub_bounds) = _include_files(&included_path, include_dirs, total_len)?;
            buffer.extend(sub_buf);

            let inc_start = sub_bounds.first()
                                      .map(|b| b.global_start)
                                      .ok_or(IncludeError::NoBoundReturned(included_path.clone()))?;
            let inc_end = sub_bounds.last()
                                    .map(|b| b.end())
                                    .ok_or(IncludeError::NoBoundReturned(included_path.clone()))?;
            let eaten_len = (buf.len() - offset) - rem.len();

            // println!("{:#?}", bounds);
            IncludeBounds::split_bounds(&mut bounds, inc_start, inc_end, eaten_len);
            bounds.extend_from_slice(&sub_bounds);
            bounds.sort();

            // println!("After split");
            // println!("{:#?}", bounds);

            buf = rem;
        }

        // no more includes, just add the rest and return
        parse_linemarkers(buf, &mut bounds, buffer.len(), 0)?;
        buffer.extend(buf);

        Ok((buffer, bounds))
    }

    _include_files(file.as_ref(),
                   &include_dirs.iter().map(|p| p.as_ref()).collect::<Vec<_>>(),
                   0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;

    #[test]
    fn linemarker_no_flag() {
        let input = b"# 1 \"<built-in>\"\n";
        assert_eq!(
            parse_linemarker(input),
            IResult::Done(
                &b""[..],
                Linemarker {
                    child_line: 1,
                    path: PathBuf::from("<built-in>"),
                    flag: None,
                }
            )
        );
    }

    #[test]
    fn linemarker_flag() {
        let input = b"# 12 \"am33xx.dtsi\" 2\n";
        assert_eq!(
            parse_linemarker(input),
            IResult::Done(
                &b""[..],
                Linemarker {
                    child_line: 12,
                    path: PathBuf::from("am33xx.dtsi"),
                    flag: Some(LinemarkerFlag::Return),
                }
            )
        );
    }
}
