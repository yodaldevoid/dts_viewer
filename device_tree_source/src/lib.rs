#[macro_use]
extern crate nom;

pub mod tree;
pub mod parser;

use nom::multispace;

use parser::escape_c_string;

named!(pub parse_include<String>, preceded!(
    tag!("/include/"),
    preceded!( multispace,
        delimited!(
            char!('"'),
            escape_c_string,
            char!('"')
        ))
));
