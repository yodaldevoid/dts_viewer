use std::str::{self, FromStr};
use std::path::{PathBuf, Path};
use std::fs::File;
use std::io::{self, Read};
use std::cmp::Ordering;

use nom::{IResult, ErrorKind, Needed, FindSubstring, digit, space, multispace, line_ending};

use parser::escape_c_string;
use ::{byte_offset_to_line_col, line_to_byte_offset};

#[derive(Debug)]
pub enum BoundsError {
    NotWithinBounds,
    IOError(io::Error),
    ParseError(::ParseError)
}

impl From<io::Error> for BoundsError {
    fn from(err: io::Error) -> Self {
        BoundsError::IOError(err)
    }
}

impl From<::ParseError> for BoundsError {
    fn from(err: ::ParseError) -> Self {
        BoundsError::ParseError(err)
    }
}

#[derive(Debug)]
pub enum IncludeError {
    NoBoundReturned(PathBuf), // No bounds returned from sub include_files
    LinemarkerInDtsi(PathBuf), // Linemarker found within DTS include
    IOError(io::Error),
    ParseError(::ParseError)
}

impl From<io::Error> for IncludeError {
    fn from(err: io::Error) -> Self {
        IncludeError::IOError(err)
    }
}

impl From<::ParseError> for IncludeError {
    fn from(err: ::ParseError) -> Self {
        IncludeError::ParseError(err)
    }
}

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
    /// This is incluive and as such the byte at the returned offset is
    /// part of this bound.
    pub fn start(&self) -> usize {
        self.global_start
    }

    /// Returns the end of the bound in the global buffer, in bytes.
    ///
    /// This is non-incluive and as such the byte at the returned offset
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
    /// Simply offseting from this position within the file does
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
            // println!("g_start: {} g_end: {}", b.global_start, b.global_end());
            if b.start() < start && b.end() >= start {
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
            } else if b.start() == start {
                // split is at begining of the bound
                // offset the start
                {
                    let offset = end - start;
                    b.global_start += offset;
                }
                // shrink the len by the offset
                b.len -= offset;
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
                    let b = File::open(&self.path)?.bytes().filter_map(|e| e.ok());
                    byte_offset_to_line_col(b, offset - self.global_start + self.child_start)
                                            .map_err(|e| e.into())
                }
                IncludeMethod::CPP => {
                    let (g_line, g_col) = byte_offset_to_line_col(global_buffer.iter(), offset)?;
                    let (s_line, s_col) = byte_offset_to_line_col(global_buffer.iter(),
                                                                  self.global_start)?;
                    let b = File::open(&self.path)?.bytes().filter_map(|e| e.ok());
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

fn parse_linemarkers(buf: &[u8], bounds: &mut Vec<IncludeBounds>, global_offset: usize)
                     -> Result<(), IncludeError> {
    let end_offset = global_offset + buf.len();
    // println!("{}", str::from_utf8(buf).unwrap());
    let mut buf = buf;
    // println!("parsing linemarkers");
    while let IResult::Done(rem, (pre, marker)) = find_linemarker(buf) {
        // println!("{}", str::from_utf8(line).unwrap());
        // println!("{:?}", marker);
        // println!("pre.len() {}", pre.len());

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
            len: rem.len(),
            method: IncludeMethod::CPP,
        };

        bounds.push(new_bound);

        buf = rem;
    }

    Ok(())
}

named!(parse_include<String>, preceded!(
    tag!("/include/"),
    preceded!( multispace,
        delimited!(
            char!('"'),
            escape_c_string,
            char!('"')
        ))
));

named!(find_include<(&[u8], String)>, do_parse!(
    pre: take_until!("/include/") >>
    path: parse_include >>
    (pre, path)
));

/// Parses include statements in the file returning a buffer with all files
/// included and the bounds of each included file.
///
/// The `IncludeBounds` can be ignored if tracing from the final buffer to the
/// original file is not needed.
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
pub fn include_files(path: &Path) -> Result<(Vec<u8>, Vec<IncludeBounds>), IncludeError> {
    fn _include_files(path: &Path,
                      main_offset: usize)
                      -> Result<(Vec<u8>, Vec<IncludeBounds>), IncludeError> {
        // TODO: check from parent directory of root file
        let mut file = File::open(path)?;
        let mut buffer: Vec<u8> = Vec::new();
        let mut bounds: Vec<IncludeBounds> = Vec::new();

        let mut string_buffer = String::new();
        file.read_to_string(&mut string_buffer)?;

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
                // TODO: check from parent directory of root file
                child_start: {
                    let b = File::open(&marker.path)?.bytes().filter_map(|e| e.ok());
                    line_to_byte_offset(b, marker.child_line)?
                },
                len: File::open(&marker.path)?.bytes().count(),
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
                // TODO: check from parent directory of root file
                len: File::open(path)?.bytes().count(),
                method: IncludeMethod::DTS,
            }
        };
        bounds.push(start_bound);

        while let IResult::Done(rem, (pre, file)) = find_include(&buf[..]) {
            parse_linemarkers(pre, &mut bounds, buffer.len())?;
            buffer.extend_from_slice(pre);

            let offset = pre.len();
            // println!("{}", file);
            // println!("Offset: {}", offset);
            // println!("{}", include_tree);

            let included_path = Path::new(&file);
            let total_len = buffer.len() + main_offset; // - 1;
            let (sub_buf, sub_bounds) = _include_files(included_path, total_len)?;
            buffer.extend(sub_buf);

            let inc_start = sub_bounds.first()
                                      .map(|b| b.global_start)
                                      .ok_or(IncludeError::NoBoundReturned(included_path.to_owned()))?;
            let inc_end = sub_bounds.last()
                                    .map(|b| b.end())
                                    .ok_or(IncludeError::NoBoundReturned(included_path.to_owned()))?;
            let eaten_len = (buf.len() - offset) - rem.len();

            IncludeBounds::split_bounds(&mut bounds, inc_start, inc_end, eaten_len);
            bounds.extend_from_slice(&sub_bounds);
            bounds.sort();

            // println!("After split");
            // println!("{}", include_tree);

            buf = rem;
        }

        // no more includes, just add the rest and return
        parse_linemarkers(buf, &mut bounds, buffer.len())?;
        buffer.extend(buf);

        Ok((buffer, bounds))
    }

    _include_files(path, 0)
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
