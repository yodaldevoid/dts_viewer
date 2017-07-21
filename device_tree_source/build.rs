extern crate walkdir;

use std::path::PathBuf;
use std::io::Write;
use std::fs::File;

use walkdir::WalkDir;

fn main() {
    println!("cargo:rerun-if-changed=tests/dts cargo:rerun-if-env-changed=DTS_DIR");

    let root_dir = ::std::env::var("DTS_DIR")
                             .map(|env| PathBuf::from(env))
                             .or_else(|_| ::std::env::var("CARGO_MANIFEST_DIR")
                                 .map(|env| PathBuf::from(env).join("tests/dts")));
    
    let mut file = File::create("tests/dts_file_names").expect("Could not create file, file_name");
    writeln!(file, "generate_tests! {{").unwrap();

    if let Ok(root_dir) = root_dir {
        let paths = WalkDir::new(root_dir.clone())
                           .follow_links(true)
                           .into_iter()
                           .filter_map(|e| e.ok())
                           .filter(|e| e.file_name().to_string_lossy().ends_with(".dts"))
                           .map(|d| d.path().to_owned());

        for path in paths {
            let name = path.file_name().unwrap()
                           .to_string_lossy()
                           .trim_right_matches(".dts")
                           .replace("+", "")
                           .replace("-", "_")
                           .replace(".", "_");

            writeln!(file, "    {} \"{}\", \"{}\",", name, root_dir.display(), path.display()).unwrap();
        }
    }

    writeln!(file, "}}").unwrap();
}
