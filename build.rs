extern crate gcc;

use std::process::Command;
use std::env;

fn main() {
	let out_dir = env::var("OUT_DIR").unwrap();

	let lexer_c_path = &format!("{}/dtc-lexer.lex.c", out_dir);
	let parser_c_path = &format!("{}/dtc-parser.tab.c", out_dir);

	Command::new("flex")
			.arg("-o")
			.arg(lexer_c_path)
			.arg("src/dtc-lexer.l")
			.status().unwrap();

	Command::new("bison")
			.arg("-d")
			.arg("--output")
			.arg(parser_c_path)
			.arg("src/dtc-parser.y")
			.status().unwrap();

	gcc::Config::new()
			.flag("-g")
			.flag("-Os")
			.flag("-fPIC")
			.flag("-Werror")
			.flag("-Wall")
			.flag("-Wpointer-arith")
			.flag("-Wcast-qual")
			.flag("-Wnested-externs")
			.flag("-Wstrict-prototypes")
			.flag("-Wmissing-prototypes")
			.flag("-Wredundant-decls")
			.flag("-Wshadow")
			.include(&out_dir)
			.include("src")
			.file(lexer_c_path)
			.file(parser_c_path)
			.compile("libdtc.a")
}
