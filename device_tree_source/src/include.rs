use std::str::{self, FromStr};
use std::path::{PathBuf, Path};
use std::fs::File;
use std::io::Read;
use std::cmp::Ordering;

use nom::{IResult, ErrorKind, Needed, FindSubstring, digit, space, multispace, line_ending};

use parser::escape_c_string;
use ::{byte_offset_to_line_col, line_to_byte_offset};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludeBounds {
    pub path: PathBuf,
    pub global_start: usize,
    pub child_start: usize,
    pub len: usize,
    pub method: IncludeMethod,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncludeMethod {
    DTS,
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
        match self.global_start.cmp(&other.global_start) {
            Equal => self.global_end().cmp(&other.global_end()),
            o => o,
        }
    }
}

impl IncludeBounds {
    pub fn global_end(&self) -> usize {
        self.global_start + self.len
    }

    pub fn split_bounds(bounds: &mut Vec<IncludeBounds>, start: usize, end: usize, offset: usize) {
        let mut remainders: Vec<IncludeBounds> = Vec::new();

        // println!("split s: {} e: {} off: {}", start, end, offset);

        for b in bounds.iter_mut() {
            // println!("g_start: {} g_end: {}", b.global_start, b.global_end());
            if b.global_start < start && b.global_end() >= start {
                // global_start -- start -- global_end
                let remainder = b.global_end() - start;

                // println!("remainder: {}", remainder);

                remainders.push(IncludeBounds {
                    path: b.path.clone(),
                    global_start: end,
                    child_start: b.child_start + start - b.global_start + offset,
                    len: remainder, // - offset,
                    method: b.method.clone(),
                });

                b.len = start - b.global_start;
            } else if b.global_start == start {
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

    pub fn file_line_from_global(&self,
                                 global_buffer: &[u8],
                                 offset: usize)
                                 -> Result<(usize, usize), String> {
        if offset >= self.global_start && offset < self.global_end() {
            match self.method {
                IncludeMethod::DTS => {
                    File::open(&self.path)
                        .map_err(|e| e.to_string())
                        .map(|f| f.bytes().filter_map(|e| e.ok()))
                        .map(|b| byte_offset_to_line_col(b, offset -
                                                            self.global_start +
                                                            self.child_start))
                }
                IncludeMethod::CPP => {
                    let (g_line, g_col) = byte_offset_to_line_col(global_buffer.iter(), offset);
                    let (s_line, s_col) = byte_offset_to_line_col(global_buffer.iter(),
                                                                  self.global_start);
                    let (c_line, c_col) = File::open(&self.path)
                                              .map_err(|e| e.to_string())
                                              .map(|f| f.bytes().filter_map(|e| e.ok()))
                                              .map(|b| byte_offset_to_line_col(b,
                                                                               self.child_start))?;

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
            Err(format!("Offset ({}) not within bounds, start: {} end: {}",
                        offset, self.global_start, self.global_end()))
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

fn parse_linemarkers(buf: &[u8], bounds: &mut Vec<IncludeBounds>, global_offset: usize) {
    let end_offset = global_offset + buf.len();
    // println!("{}", str::from_utf8(buf).unwrap());
    let mut buf = buf;
    // println!("parsing linemarkers");
    while let IResult::Done(rem, (pre, marker)) = find_linemarker(buf) {
        // println!("{}", str::from_utf8(line).unwrap());
        // println!("{:?}", marker);
        // println!("pre.len() {}", pre.len());

        // double check that last bound was from a linemarker
        match bounds.last() {
            Some(&IncludeBounds { method: IncludeMethod::CPP, .. }) => {}
            _ => {
                println!("{:#?}", bounds);
                panic!("Linemarker found within DTS include")
            }
        }

        // end last
        bounds.last_mut().unwrap().len = pre.len();

        // start at new line
        let new_bound = IncludeBounds {
            path: marker.path.clone(),
            global_start: end_offset - rem.len(),
            child_start: File::open(&marker.path)
                            .map(|f| f.bytes().filter_map(|e| e.ok()))
                            .map(|b| line_to_byte_offset(b, marker.child_line).unwrap()) //TODO: unwraping is bad, SOK?
                            .unwrap_or(0),
            len: rem.len(),
            method: IncludeMethod::CPP,
        };

        bounds.push(new_bound);

        buf = rem;
    }
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

pub fn include_files(path: &Path,
                     main_offset: usize)
                     -> Result<(Vec<u8>, Vec<IncludeBounds>), String> {
    // TODO: check from parent directory of root file
    let mut file = File::open(path).unwrap();
    let mut buffer: Vec<u8> = Vec::new();
    let mut bounds: Vec<IncludeBounds> = Vec::new();

    let mut string_buffer = String::new();
    file.read_to_string(&mut string_buffer).map_err(|_| "IO Error".to_string())?;

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
            child_start: File::open(&marker.path)
                             .map(|f| f.bytes().filter_map(|e| e.ok()))
                             .map(|b| line_to_byte_offset(b, marker.child_line).unwrap()) //TODO: unwraping is bad, SOK?
                             .unwrap_or(0),
            len: File::open(&marker.path).unwrap().bytes().count(),
            method: IncludeMethod::CPP,
        };

        buffer.extend_from_slice(line);
        buf = rem;

        bound
    } else {
        // println!("main_offset {}", main_offset);
        IncludeBounds {
            path: path.to_path_buf(),
            global_start: main_offset,
            child_start: 0,
            // TODO: check from parent directory of root file
            len: File::open(path).unwrap().bytes().count(),
            method: IncludeMethod::DTS,
        }
    };
    bounds.push(start_bound);

    while let IResult::Done(rem, (pre, file)) = find_include(&buf[..]) {
        parse_linemarkers(pre, &mut bounds, buffer.len());
        buffer.extend_from_slice(pre);

        let offset = pre.len();
        // println!("{}", file);
        // println!("Offset: {}", offset);
        // println!("{}", include_tree);

        let included_path = Path::new(&file);
        let total_len = buffer.len() + main_offset; // - 1;
        let (sub_buf, sub_bounds) = include_files(included_path, total_len)?;
        buffer.extend(sub_buf);

        let inc_start = sub_bounds.first()
                                  .map(|b| b.global_start)
                                  .expect(&format!("No bounds returned: {}",
                                                  included_path.to_string_lossy()));
        let inc_end = sub_bounds.last()
                                .map(|b| b.global_end())
                                .expect(&format!("No bounds returned: {}",
                                                included_path.to_string_lossy()));
        let eaten_len = (buf.len() - offset) - rem.len();
        //include_tree.offset_after_location(inc_start,
        //                                   inc_end as isize - inc_start as isize -
        //                                   eaten_len as isize);
        // println!("After offset");
        // println!("{}", include_tree);
        IncludeBounds::split_bounds(&mut bounds, inc_start, inc_end, eaten_len);
        bounds.extend_from_slice(&sub_bounds);
        bounds.sort();

        // println!("After split");
        // println!("{}", include_tree);

        buf = rem;
    }

    // no more includes, just add the rest and return
    parse_linemarkers(buf, &mut bounds, buffer.len());
    buffer.extend(buf);

    Ok((buffer, bounds))
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
