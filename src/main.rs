extern crate device_tree_source;
#[macro_use]
extern crate clap;
extern crate mktemp;

mod change_tracker;

use std::process::Command;
use std::path::{Path, PathBuf};
use std::io::{self, BufRead, Write};
use std::iter::Iterator;
use std::fmt::{self, Display, Formatter};

use mktemp::Temp;

use device_tree_source::parser::{ParseResult, parse_dt};
use device_tree_source::tree::Offset;
use device_tree_source::include::{IncludeBounds, IncludeMethod, IncludeError, BoundsError,
                                  include_files, get_bounds_containing_offset};

use change_tracker::LabelStore;

// General idea:
//  Run CPP
//  Parse file for DTS includes and replace with include contents
//  Find byte starts/ends for each file
//  Parse file to create device tree
//  create mapping of objects to byte offset starting points
//  - not perfect with offsets stored in object, but it will do
//  create mapping of full paths to objects
//  create mapping of labels to full paths
//  Parse device tree to create changes
//  Use mapping and file starts to pair changes to file byte offsets
//  Turn file byte offsets to file lines/cols
//  TODO: ???
//  TODO: Profit!
//  Oh, and somehow display the damn information
fn main() {
    let matches = clap_app!(dts_viewer =>
            (version: crate_version!())
            (author: "Gabriel S. <ga29smith@gmail.com>")
            (@arg file: +required "DTS file to parse")
            (@arg no_defaults: -n --no_defaults "Disable default includes. \
                An 'include' directory, if it exists, is automatically included")
            (@arg include: -I ... +takes_value "Additional files to pass to CPP as an include")
        )
        .get_matches();

    let file_name = matches.value_of("file").unwrap();
    let mut cpp_temp_out = Temp::new_file().expect("Could not create temp file");
    let mut include_dirs = Vec::new();

    let mut cpp_command = Command::new("gcc");
    cpp_command.args(&["-E", "-nostdinc"])
        .args(&["-undef", "-D__DTS__", "-x", "assembler-with-cpp"])
        .args(&["-o", cpp_temp_out.as_ref().to_str().unwrap()])
        .arg(&file_name);

    if !matches.is_present("no_defaults") {
        cpp_command.args(&["-I", "."]);
        include_dirs.push(PathBuf::from("."));
        if Path::new("include").is_dir() {
            cpp_command.args(&["-I", "include/"]);
            include_dirs.push(PathBuf::from("include/"));
        }

        if let Some(parent) = Path::new(file_name).parent() {
            cpp_command.args(&["-I", &parent.to_string_lossy()]);
            include_dirs.push(parent.to_owned());

            let include = parent.join("include");
            if include.is_dir() {
                cpp_command.args(&["-I", &include.to_string_lossy()]);
                include_dirs.push(include);
            }
        } else {
            println!("Could not get parent directory of file");
        }
    }

    if let Some(includes) = matches.values_of("include") {
        for include in includes {
            if Path::new(include).is_dir() {
                cpp_command.args(&["-I", include]);
                include_dirs.push(PathBuf::from(include));
            }
        }
    }

    // println!("{:?}", cpp_command);

    let include_output = cpp_command.output().expect("Failed to start CPP");
    if !include_output.status.success() {
        // Done to prevent a panic as the file will not have been written to
        cpp_temp_out.release();
        println!("Failed to execute CPP. Error message is below.");
        print!("{}", String::from_utf8_lossy(&include_output.stderr));
        return;
    }

    let (buffer, bounds) = match include_files(&cpp_temp_out, &include_dirs) {
        Ok(x) => x,
        Err(e) => {
            match e {
                IncludeError::IOError(err, path) => {
                    print!("IO error: {}", err);
                    if let Some(path) = path {
                        println!(" {}", path.display());
                    }
                }
                IncludeError::LinemarkerInDtsi(path) =>
                    println!("Extraneous linemarker found in DT include: {}",
                             path.to_string_lossy()),
                IncludeError::ParseError(_) =>
                    println!("Failed to convert line to byte offset for bounds tracking."),
                IncludeError::NoBoundReturned(path) =>
                    println!("No bounds returned after parsing file: {}", path.to_string_lossy()),
            }
            return;
        }
    };

    // println!("{:#?}", bounds);
    // {
    //     use std::fs::File;
    //     let mut total_dts_dump = File::create("total_dts_dump.dts").unwrap();
    //     total_dts_dump.write_all(&buffer).unwrap()
    // }

    let include_tree = IncludeTree::bounds_to_tree(&bounds);
    if let Some(tree) = include_tree {
        println!("{}", tree);
    } else {
        println!("-- Could not constuct include tree from bounds!");
    }

    let (dt_info, amends) = match parse_dt(&buffer) {
        Ok(ParseResult::Complete(dt_info, amends)) => (dt_info, amends),
        Ok(ParseResult::RemainingInput(dt_info, amends, rem)) => {
            println!("Input remaining after parsing:\n\"{}\"", String::from_utf8_lossy(rem));
            (dt_info, amends)
        }
        Err(err) => {
            println!("{:?}", err);
            return;
        }
    };

    // TODO: perform secondary checks and jazz (only smooth) - issue 9

    let mut store = LabelStore::new();
    store.fill(&dt_info, &amends);

    loop {
        print!("Enter alias or path: ");
        io::stdout().flush().expect("Error flushing stdout");

        let mut line = String::new();
        let stdin = io::stdin();
        stdin.lock().read_line(&mut line).expect("Error reading from stdin");
        let line = line.trim();

        if line.is_empty() {
            break;
        }

        let path = if line.starts_with('/') {
            Some(PathBuf::from(line))
        } else {
            let line = if line.starts_with('&') {
                &line[1..]
            } else {
                line
            };
            // TODO:
            // if label covers multiple paths (after saving old aliases is implemented)
            //     ask user which one they want as well as why this is being asked
            // show all changes along the way
            match store.path_from_label(line) {
                Some(path) => {
                    println!("Path: {}", path.to_string_lossy());
                    Some(path.to_owned())
                }
                None => {
                    println!("Label points to no path");
                    None
                }
            }
        };

        if let Some(path) = path {
            match store.changes_from_path(&path) {
                Some(changes) => {
                    for change in changes {
                        let offset = change.get_offset();
                        // println!("Start offset: {}", offset);
                        match get_bounds_containing_offset(&bounds, offset) {
                            Ok(bound) => {
                                print!("File: {}", bound.child_path().to_string_lossy());

                                match bound.file_line_from_global(&buffer, offset) {
                                    Ok((line, col)) => {
                                        println!(", Line: {}, Column: {}", line, col)
                                    }
                                    Err(err) => match err {
                                        BoundsError::ParseError(_) =>
                                            println!("Offset ({}) could not be converted to line.",
                                                     offset),
                                        BoundsError::IOError(..) =>
                                            println!("Failed to open file: {}",
                                                     bound.child_path().to_string_lossy()),
                                        BoundsError::NotWithinBounds =>
                                            println!("File offset ({}) was supposed to be in\
                                                         bound. {:?}",
                                                     offset,
                                                     bound),
                                    }
                                }
                            }
                            Err(_) => println!("-- Could not find file for offset {}", offset),
                        }
                        println!("{}\n", change);
                    }
                }
                None => println!("Nothing at path"),
            }
        }
    }
}

#[derive(Debug)]
struct IncludeTree {
    path: PathBuf,
    method: IncludeMethod,
    includes: Vec<IncludeTree>,
}

impl IncludeTree {
    fn bounds_to_tree(bounds: &[IncludeBounds]) -> Option<IncludeTree> {
        if let Some(first) = bounds.first() {
            let mut tree = IncludeTree {
                path: first.child_path().to_owned(),
                includes: Vec::new(),
                method: first.include_method().clone(),
            };

            //TODO: we don't really need the filter, benchmark speed w/wo
            for sub_bounds in bounds.split(|b| b.child_path() == first.child_path())
                                    .filter(|s| !s.is_empty()) {
                if let Some(sub_tree) = Self::bounds_to_tree(sub_bounds) {
                    tree.includes.push(sub_tree);
                }
            }

            Some(tree)
        } else {
            None
        }
    }

    fn write(&self, f: &mut Formatter, prefix: &str) -> fmt::Result {
        let mut next_prefix = prefix.to_owned();
        next_prefix.push_str(" |-");

        let method = match self.method {
            IncludeMethod::CPP => "CPP",
            IncludeMethod::DTS => "DTS",
        };
        writeln!(f, "{} {}: {}", prefix, self.path.to_string_lossy(), method)?;
        for t in &self.includes {
            t.write(f, &next_prefix)?;
        }

        Ok(())
    }
}

impl Display for IncludeTree {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.write(f, "")
    }
}
