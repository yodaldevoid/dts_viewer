use std::path::{Path, PathBuf};
use std::fs::File;
use std::str::Lines;
use std::io::{ Read, BufRead, BufReader };
use std::iter::Peekable;

use nom::IResult;

use inner_tree::*;
use dts_parser::parse_include;

#[derive(Debug)]
pub enum LinemarkerFlag {
    Start,
    Return,
    System,
    Extern,
}

pub fn parse_cpp_outputs<'a>(cpp_stderr: &'a str,
                             cpp_output: &Path,
                             root_file_name: &str)
                             -> Result<(ParsedFile, Vec<u8>), String> {
    let mut include_tree = ParsedFile::new(PathBuf::from(&root_file_name), IncludeMethod::CPP);
    parse_cpp_stderr(&mut cpp_stderr.lines().peekable(), &mut include_tree, 0);
    // println!("{:#?}", include_tree);
    parse_cpp_file(cpp_output, &mut include_tree);
    // println!("{:#?}", include_tree);
    let result = include_dts_files(cpp_output, &mut include_tree, 0)?;
    Ok((include_tree, result))
}

// parse stderr to get the include tree
fn parse_cpp_stderr(lines: &mut Peekable<Lines>, parent_file: &mut ParsedFile, depth: usize) {
    while let Some(line) = lines.peek().cloned() {

        let mut count: usize = 0;
        for sc in line.chars() {
            if sc == '.' {
                count += 1;
            } else {
                break;
            }
        }
        let count = count;

        if count == 0 || count < depth || count == depth {
            return;
        } else if count > depth && (count - depth) == 1 {
            parent_file.add_include(ParsedFile::new(PathBuf::from(lines.next()
                                                        .unwrap()
                                                        .trim_left_matches('.')
                                                        .trim_left()),
                                                    IncludeMethod::CPP));
            parse_cpp_stderr(lines,
                             parent_file.included_files.last_mut().unwrap(),
                             depth + 1);
        } else {
            unreachable!();
        }
    }
}

// parse preprocessed file to find starts (and stops) of included files
fn parse_cpp_file(cpp_output: &Path, include_tree: &mut ParsedFile) {
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
            let child_num: usize = tokens[0].parse().unwrap();  //TODO: check length before assuming
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
        parse_cpp_linemarkers(&parsed_lines.next(),
                              &parsed_lines.peek(),
                              Path::new(cpp_output))
            .unwrap() {
        if let Some(m) = mapping {
            include_tree.assign_mapping(path, m).expect("Failed to assign mapping");
        }
    }
}

fn parse_cpp_linemarkers<'a>(current: &'a Option<(usize, usize, PathBuf, Option<LinemarkerFlag>)>,
                             next: &Option<&(usize, usize, PathBuf, Option<LinemarkerFlag>)>,
                             cpp_output: &Path)
                             -> Result<Option<(&'a Path, Option<FileMapping>)>, String> {
    if let Some((c_line_num, c_child_num, ref c_path, _)) = *current {
        if let Some(&(n_line_num, _, _, _)) = *next {
            Ok(Some((c_path,
                     if n_line_num - c_line_num > 1 {
                         Some(FileMapping {
                             parent_start:
                                 line_to_byte_offset(File::open(cpp_output)
                                                         .expect(&format!("File not found: {}",
                                                                          cpp_output.to_str()
                                                                              .unwrap()))
                                                         .bytes()
                                                         .filter_map(|e| e.ok()),
                                                     c_line_num + 1)?,
                             child_start:
                                 line_to_byte_offset(File::open(c_path)
                                                         .expect(&format!("File not found: {}",
                                                                          c_path.to_str()
                                                                              .unwrap()))
                                                         .bytes()
                                                         .filter_map(|e| e.ok()),
                                                     c_child_num)?,
                             len: line_to_byte_offset(File::open(cpp_output)
                                                          .expect(&format!("File not found: {}",
                                                                           cpp_output.to_str()
                                                                               .unwrap()))
                                                          .bytes()
                                                          .filter_map(|e| e.ok()),
                                                      n_line_num)? -
                                  line_to_byte_offset(File::open(cpp_output)
                                                          .expect(&format!("File not found: {}",
                                                                           cpp_output.to_str()
                                                                               .unwrap()))
                                                          .bytes()
                                                          .filter_map(|e| e.ok()),
                                                      c_line_num + 1)?,
                         })
                     } else {
                         None
                     })))
        } else {
            let last_byte = File::open(cpp_output).unwrap().bytes().count();
            Ok(Some((c_path,
                     Some(FileMapping {
                         parent_start: line_to_byte_offset(File::open(cpp_output)
                                                               .unwrap()
                                                               .bytes()
                                                               .filter_map(|e| e.ok()),
                                                           c_line_num + 1)?,
                         child_start: line_to_byte_offset(File::open(c_path)
                                                              .unwrap()
                                                              .bytes()
                                                              .filter_map(|e| e.ok()),
                                                          c_child_num)?,
                         len: last_byte -
                              line_to_byte_offset(File::open(cpp_output)
                                                      .unwrap()
                                                      .bytes()
                                                      .filter_map(|e| e.ok()),
                                                  c_line_num + 1)?,
                     }))))
        }
    } else {
        Ok(None)
    }
}

fn include_dts_files(file: &Path,
                     include_tree: &mut ParsedFile,
                     main_offset: usize)
                     -> Result<Vec<u8>, String> {
    let mut file = File::open(file).unwrap();
    let mut buffer: Vec<u8> = Vec::new();

    let mut string_buffer = String::new();
    file.read_to_string(&mut string_buffer).map_err(|_| "IO Error".to_string())?;

    let mut buf = string_buffer.as_bytes();
    loop {
        // go until /include/
        buf = if let Some(offset) = buf.windows(9).position(|sub| sub == b"/include/") {
            buffer.extend_from_slice(&buf[..offset]);
            if let IResult::Done(rem, file) = parse_include(&buf[offset..]) {
                // println!("{}", file);
                // println!("Offset: {}", offset);
                // println!("{}", include_tree);

                let eaten_len = (buf.len() - offset) - rem.len();

                let included_path = Path::new(&file);
                let mut included_file = ParsedFile::new(included_path.to_path_buf(),
                                                        IncludeMethod::DTS);
                let total_len = buffer.len() + main_offset;
                included_file.mappings.push(FileMapping {
                    parent_start: total_len,
                    child_start: 0,
                    // TODO: check from parent directory of root file
                    len: File::open(&included_path).unwrap().bytes().count(),
                });
                buffer.extend(include_dts_files(included_path, &mut included_file, total_len)?);

                let (inc_start, inc_end) = included_file.bounds_of_tree()?;
                include_tree.offset_after_location(inc_start, inc_end as isize - inc_start as isize);
                // println!("After offset");
                // println!("{}", include_tree);

                {
                    let inc_file = include_tree.file_from_offset_mut(inc_start)?;
                    inc_file.split_mappings(inc_start, inc_end, eaten_len);
                    inc_file.add_include(included_file);
                }
                // println!("After split");
                // println!("{}", include_tree);

                rem
            } else {
                buffer.extend_from_slice(&buf[offset..offset + 9]);
                &buf[offset + 9..]
            }
        } else {
            // no more includes, just add the rest and return
            buffer.extend(buf);
            return Ok(buffer);
        };
    }
}
