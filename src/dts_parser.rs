use std::str::{self, FromStr};
use nom::{IResult, ErrorKind, hex_digit, oct_digit, digit, is_alphanumeric,
		alpha, line_ending, not_line_ending, multispace, space};
use std::num::ParseIntError;

// Copied and modified from rust-lang/rust/src/libcore/num/mod.rs
trait FromStrRadix: PartialOrd + Copy {
	type Err;
	fn from_str_radix(s: &str, radix: u32) -> Result<Self, Self::Err>;
}

macro_rules! doit {
	($($t:ty)*) => ($(impl FromStrRadix for $t {
		type Err = ParseIntError;
		fn from_str_radix(s: &str, r: u32) -> Result<$t, Self::Err> { Self::from_str_radix(s, r) }
	})*)
}
doit! { i8 i16 i32 i64 isize u8 u16 u32 u64 usize }

trait Labeled {
	fn add_label(&mut self, label: &str);
}

#[derive(PartialEq, Debug)]
pub struct BootInfo {
	reserve_info: Vec<ReserveInfo>,
	boot_cpuid: u32,
	root: Node,
}

#[derive(PartialEq, Debug)]
pub struct ReserveInfo {
	address: u64,
	size: u64,
	labels: Vec<String>,
}

impl Labeled for ReserveInfo {
	fn add_label(&mut self, label: &str) {
		let label = label.to_string();
		if !self.labels.contains(&label) {
			self.labels.push(label);
		}
	}
}

#[derive(PartialEq, Debug)]
pub struct Node {
	deleted: bool,
	name: String,
	proplist: Vec<Property>,
	children: Vec<Node>,

	//fullpath: Option<PathBuf>,
	//length to the # part of node_name@#
	//basenamelen: usize,

	//phandle: u32,
	//addr_cells: i32,
	//size_cells: i32,

	labels: Vec<String>,
}

impl Labeled for Node {
	fn add_label(&mut self, label: &str) {
		let label = label.to_string();
		if !self.labels.contains(&label) {
			self.labels.push(label);
		}
	}
}

#[derive(PartialEq, Debug)]
pub struct Property {
	deleted: bool,
	name: String,
	val: Option<Vec<Data>>,
	labels: Vec<String>,
}

impl Labeled for Property {
	fn add_label(&mut self, label: &str) {
		let label = label.to_string();
		if !self.labels.contains(&label) {
			self.labels.push(label);
		}
	}
}

impl Property {
	fn delete(&mut self) {
		self.deleted = true;
		self.labels.clear();
	}
}

#[derive(PartialEq, Debug)]
pub enum Data {
	Reference(String),
	String(String),
	Cells(usize, Vec<(u64, Option<String>)>),
	ByteArray(Vec<u8>),
}

fn from_str_hex<T: FromStrRadix>(s: &str) -> Result<T ,T::Err> {
	T::from_str_radix(s, 16)
}

fn from_str_oct<T: FromStrRadix>(s: &str) -> Result<T ,T::Err> {
	T::from_str_radix(s, 8)
}
fn from_str_dec<T: FromStr>(s: &str) -> Result<T ,T::Err> {
	T::from_str(s)
}

/* 
 * This is dumb and feels wrong, but it works so I will complain no more.
 * Thank you to Filipe Gonçalves and https://crates.io/crates/config for the inspiration.
 */
named!(eat_junk, do_parse!(many0!(alt!(
	delimited!(tag!("/*"), take_until!("*/"), tag!("*/")) |
	delimited!(tag!("//"), not_line_ending, line_ending) |
	do_parse!( //TODO: maybe actually parse the info. Or not.
		tag!("#") >>
		opt!(tag!("line")) >>
		many1!(space) >>
		many1!(digit) >>
		many1!(space) >>
		string: not_line_ending >>
		line_ending >>
		(string)
	) |
	multispace
)) >> (&b""[..])));

macro_rules! comments_ws (
	($i:expr, $($args:tt)*) => ( {
		use $crate::dts_parser::eat_junk;
		sep!($i, eat_junk, $($args)*)
	} )
);

named!(opr_infix<OprInfix>, alt_complete!(
	tag!("*") => { |_| OprInfix::Multiply } |
	tag!("/") => { |_| OprInfix::Divide } |
	tag!("%") => { |_| OprInfix::Modulus } |

	tag!("+") => { |_| OprInfix::Add } |
	tag!("-") => { |_| OprInfix::Subtract } |

	tag!("<<") => { |_| OprInfix::LeftShift } |
	tag!(">>") => { |_| OprInfix::RightShift } |

	tag!("<=") => { |_| OprInfix::LesserEqual } |
	tag!(">=") => { |_| OprInfix::GreaterEqual } |
	tag!("<") => { |_| OprInfix::Lesser } |
	tag!(">") => { |_| OprInfix::Greater } |
	tag!("==") => { |_| OprInfix::Equal } |
	tag!("!=") => { |_| OprInfix::NotEqual } |

	tag!("&&") => { |_| OprInfix::And } |
	tag!("||") => { |_| OprInfix::Or } |

	tag!("&") => { |_| OprInfix::BitAnd } |
	tag!("^") => { |_| OprInfix::BitXor } |
	tag!("|") => { |_| OprInfix::BitOr }
));

#[derive(PartialEq, Debug)]
enum OprInfix {
	Multiply,
	Divide,
	Modulus,

	Add,
	Subtract,

	LeftShift,
	RightShift,

	Lesser,
	Greater,
	LesserEqual,
	GreaterEqual,
	Equal,
	NotEqual,

	BitAnd,
	BitXor,
	BitOr,

	And,
	Or,
}

impl OprInfix {
	fn apply(&self, a: u64, b: u64) -> u64 {
		match *self {
			OprInfix::Multiply => a * b,
			OprInfix::Divide => a / b,
			OprInfix::Modulus => a % b,

			OprInfix::Add => a + b,
			OprInfix::Subtract => a - b,

			OprInfix::LeftShift => a << b,
			OprInfix::RightShift => a >> b,

			OprInfix::Lesser => if a < b { 1 } else { 0 },
			OprInfix::Greater => if a > b { 1 } else { 0 },
			OprInfix::LesserEqual => if a <= b { 1 } else { 0 },
			OprInfix::GreaterEqual => if a >= b { 1 } else { 0 },
			OprInfix::Equal => if a == b { 1 } else { 0 },
			OprInfix::NotEqual => if a != b { 1 } else { 0 },

			OprInfix::BitAnd => a & b,
			OprInfix::BitXor => a ^ b,
			OprInfix::BitOr => a | b,

			OprInfix::And => if a != 0 && b != 0 { 1 } else { 0 },
			OprInfix::Or => if a != 0 || b != 0 { 1 } else { 0 },
		}
	}
}

named!(opr_prefix<OprPrefix>, alt_complete!(
	tag!("!") => { |_| OprPrefix::Not } |
	tag!("~") => { |_| OprPrefix::BitNot } |
	tag!("-") => { |_| OprPrefix::Negate }
));

#[derive(PartialEq, Debug)]
enum OprPrefix {
	Negate,
	BitNot,
	Not,
}

impl OprPrefix {
	fn apply(&self, a: u64) -> u64 {
		match *self {
			OprPrefix::Negate => a.wrapping_neg(),
			OprPrefix::BitNot => a ^ u64::max_value(),
			OprPrefix::Not => if a == 0 { 1 } else { 0 },
		}
	}
}

#[derive(Debug)]
enum Token {
	Number(u64),
	Prefix(OprPrefix),
	Infix(OprInfix),
	Paren
}

pub fn parse_c_expr(input: &[u8]) -> IResult<&[u8], u64> {
	let mut stack = Vec::new();
	//println!("Input: {:?}", String::from_utf8_lossy(&input[..20]));
	let mut buf = input;
	loop {
		//println!("Stack: {:?}", stack);
		buf = if let IResult::Done(cleaned, _) = eat_junk(buf) {
			//println!("cleaned: {}", String::from_utf8_lossy(cleaned));
			if let IResult::Done(matched, num) = integer(cleaned) {
				match stack.pop() {
					None => {
						//println!("Pop Nothing");
						stack.push(Token::Number(num));
					},
					Some(Token::Paren) => {
						//println!("Pop Paren");
						stack.push(Token::Paren);
						stack.push(Token::Number(num));
					},
					Some(Token::Prefix(ref x)) => {
						//println!("Pop Prefix");
						let num = x.apply(num);
						stack.push(Token::Number(num));
					},
					Some(Token::Infix(ref x)) => {
						//println!("Pop Infix");
						if let Some(Token::Number(a)) = stack.pop() {
							let num = x.apply(a, num);
							stack.push(Token::Number(num));
						} else {
							return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
						}
					},
					Some(Token::Number(a)) if stack.is_empty() => return IResult::Done(buf, a),
					_ => return IResult::Error(error_position!(ErrorKind::Custom(1), buf)),
				};

				matched
			} else if let IResult::Done(matched, tok) = opr_infix(cleaned) {
				//println!("Infix: {:?}", tok);
				if let Some(&Token::Number(a)) = stack.last() {
					if (tok == OprInfix::Greater || tok == OprInfix::BitAnd) && stack.len() == 1 {
						if let IResult::Done(cleaned, _) = eat_junk(matched) {
							let test = closure!(preceded!(
								eat_junk,
								alt!(
									tag!("(") |
									recognize!(opr_prefix) |
									recognize!(integer)
							)));
							match test(cleaned) {
								IResult::Done(_, _) => {},
								_ => return IResult::Done(buf, a),
							}
						}
					}
					stack.push(Token::Infix(tok));
				} else if tok == OprInfix::Subtract {
					stack.push(Token::Prefix(OprPrefix::Negate));
				} else {
					return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
				};

				matched
			} else if let IResult::Done(matched, tok) = opr_prefix(cleaned) {
				//println!("Prefix: {:?}", tok);
				if let Some(&Token::Number(a)) = stack.last() {
					if stack.len() == 1 {
						return IResult::Done(buf, a)
					}
					return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
				} else {
					stack.push(Token::Prefix(tok));
				};

				matched
			} else if let Some(&c) = cleaned.first() {
				match c {
					b'(' => {
						//println!("Paren");
						match stack.pop() {
							None => {
								stack.push(Token::Paren);
							},
							Some(x @ Token::Paren) |
							Some(x @ Token::Prefix(_)) |
							Some(x @ Token::Infix(_)) => {
								stack.push(x);
								stack.push(Token::Paren);
							},
							Some(Token::Number(a)) => if stack.is_empty() {
								return IResult::Done(buf, a)
							} else {
								return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
							},
						};
					},

					b')' => {
						//println!("Close");
						if let Some(Token::Number(num)) = stack.pop() {
							if let Some(Token::Paren) = stack.pop() {
								match stack.pop() {
									None => {
										stack.push(Token::Number(num));
									},
									Some(Token::Paren) => {
										stack.push(Token::Paren);
										stack.push(Token::Number(num));
									},
									Some(Token::Prefix(ref x)) => {
										let num = x.apply(num);
										stack.push(Token::Number(num));
									},
									Some(Token::Infix(ref x)) => {
										if let Some(Token::Number(a)) = stack.pop() {
											let num = x.apply(a, num);
											stack.push(Token::Number(num));
										} else {
											return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
										}
									},
									_ => return IResult::Error(error_position!(ErrorKind::Custom(1), buf)),
								};
							} else {
								return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
							}
						} else {
							return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
						}
					},

					x => { println!("Unimplemented char: {}", x as char); return IResult::Error(error_position!(ErrorKind::Custom(1), buf)) },
				}

				&cleaned[1..]
			} else {
				//Incomplete
				println!("Incomplete: {}", String::from_utf8_lossy(buf));
				unreachable!()
			}
		} else {
			//println!("Junk eating failed!?");
			unreachable!()
		};

		if buf.len() == 0 {
			if stack.len() == 1 {
				if let Some(&Token::Number(num)) = stack.last() {
					return IResult::Done(buf, num)
				} else {
					return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
				}
			} else {
				return IResult::Error(error_position!(ErrorKind::Custom(1), buf))
			}
		}
	}
}

named!(pub integer<u64>, alt_complete!(
/*
	comments_ws!(do_parse!( // trinary
		a: flat_map!(take_until_and_consume!("?"), integer) >>
		b: flat_map!(take_until_and_consume!(":"), integer) >>
		c: integer >>
		(if a != 0 { b } else { c })
	)) |
*/
	complete!(preceded!(tag_no_case!("0x"), map_res!(map_res!(hex_digit, str::from_utf8), from_str_hex::<u64>))) |
	preceded!(tag_no_case!("0"), map_res!(map_res!(oct_digit, str::from_utf8), from_str_oct::<u64>)) |
	map_res!(map_res!(digit, str::from_utf8), from_str_dec::<u64>)
));

fn is_prop_node_char(c: u8) -> bool {
	is_alphanumeric(c) ||
	c == b',' || c == b'.' || c == b'_' ||
	c == b'+' || c == b'*' || c == b'#' ||
	c == b'?' || c == b'@' || c == b'-'
}

fn is_path_char(c: u8) -> bool {
	is_prop_node_char(c) || c == b'/'
}

fn is_label_char(c: u8) -> bool {
	is_alphanumeric(c) || c == b'_'
}

named!(parse_label<String>,
	map!(map_res!(
		recognize!(preceded!(alt!(alpha | tag!("_")), take_while!(is_label_char))),
	str::from_utf8), String::from)
);

named!(parse_ref<String>, alt!(
	preceded!(
		char!('&'),
		delimited!(
			char!('{'),
			map!(map_res!(take_while1!(is_path_char), str::from_utf8), String::from),
			char!('}')
		)
	) |
	preceded!(
		char!('&'),
		map!(map_res!(take_while1!(is_label_char), str::from_utf8), String::from)
	)
));

// Warning! Only supports hex escape codes up to 0x7f because UTF-8 reasons
named!(escape_c_string<String>, map_res!(
	escaped_transform!(take_until_either!("\\\""), '\\',
		alt!(
			tag!("a")	=> { |_| vec![b'\x07'] } |
			tag!("b")	=> { |_| vec![b'\x08'] } |
			tag!("t")	=> { |_| vec![b'\t'] } |
			tag!("n")	=> { |_| vec![b'\n'] } |
			tag!("v")	=> { |_| vec![b'\x0B'] } |
			tag!("f")	=> { |_| vec![b'\x0C'] } |
			tag!("r")	=> { |_| vec![b'\r'] } |
			tag!("\\")	=> { |_| vec![b'\\'] } |
			tag!("\"")	=> { |_| vec![b'\"'] } |
			preceded!(
				tag_no_case!("x"),
				map_res!(map_res!(hex_digit, str::from_utf8), from_str_hex::<u8>)
			) => { |c| vec![c] } |
			map_res!(map_res!(oct_digit, str::from_utf8), from_str_oct::<u8>) => { |c| vec![c] }
		)),
	String::from_utf8)
);

// Can support hex escape codes up to 0xff because we are not converting to char
named!(escape_c_char<u8>, alt!(
	tag!("\\a")		=> { |_| 0x07 } |
	tag!("\\b")		=> { |_| 0x08 } |
	tag!("\\t")		=> { |_| b'\t' } |
	tag!("\\n")		=> { |_| b'\n' } |
	tag!("\\v")		=> { |_| 0x0B } |
	tag!("\\f")		=> { |_| 0x0C } |
	tag!("\\r")		=> { |_| b'\r' } |
	tag!("\\\\")	=> { |_| b'\\' } |
	tag!("\\\'")	=> { |_| b'\'' } |
	preceded!(
		tag_no_case!("\\x"),
		map_res!(map_res!(hex_digit, str::from_utf8), from_str_hex::<u8>)
	) |
	preceded!(tag!("\\"), map_res!(map_res!(oct_digit, str::from_utf8), from_str_oct::<u8>)) |
	take!(1) => { |c: &[u8]| c[0] }
));

pub fn parse_cell(input: &[u8], bits: usize) -> IResult<&[u8], (u64, Option<String>)> {
	if bits != 8 && bits != 16 && bits != 32 && bits != 64 {
		return IResult::Error(error_position!(ErrorKind::Custom(1), input));
	}

	let parse_cell_internal = closure!(do_parse!(
		num: opt!(alt!(
			parse_c_expr |
			map!(delimited!(
				char!('\''),
				escape_c_char,
				char!('\'')
			), u64::from)
		)) >>
		r: cond!(bits == 32 && num.is_none(), parse_ref) >>
		(num.unwrap_or(0), r)
	));

	let result = parse_cell_internal(input);
	if bits != 64 {
		if let IResult::Done(_,(num, _)) = result {
			let mask = (1 << bits) - 1;
			if (num > mask) && ((num | mask) != u64::max_value()) {
				return IResult::Error(error_position!(ErrorKind::Custom(1), input));
			}
		}
	}

	result
}

named!(parse_mem_reserve<ReserveInfo>, comments_ws!(do_parse!(
	labels: many0!(terminated!(parse_label, char!(':'))) >>
	tag!("/memreserve/") >>
	addr: parse_c_expr >>
	size: parse_c_expr >>
	char!(';') >>
	(ReserveInfo { address: addr, size: size, labels: labels })
)));

//TODO: labels in data
//TODO: include binary
named!(pub parse_data<Data>, comments_ws!(alt!(
	delimited!(
		char!('"'),
		do_parse!(
			val: escape_c_string >>
			(Data::String(val))
		),
		char!('"')
	) |
	do_parse!(
		bits: opt!(comments_ws!(preceded!(
			tag!("/bits/"),
			flat_map!(take_until!("<"), integer)
		))) >>
		char!('<') >>
		val: separated_nonempty_list!(eat_junk, apply!(parse_cell, bits.unwrap_or(32) as usize)) >>
		char!('>') >>
		( Data::Cells(bits.unwrap_or(32) as usize, val) )
	) |
	delimited!(
		char!('['),
		do_parse!(
			val: many1!(map_res!(map_res!(comments_ws!(take!(2)), str::from_utf8), from_str_hex::<u8>)) >>
			(Data::ByteArray(val))
		),
		char!(']')
	) |
	do_parse!(val: parse_ref >> (Data::Reference(val)))
)));

named!(pub parse_prop<Property>, comments_ws!(do_parse!(
	labels: many0!(terminated!(parse_label, char!(':'))) >>
	name: map!(map_res!(take_while1!(is_prop_node_char), str::from_utf8), String::from) >>
	data: opt!(preceded!(char!('='), separated_nonempty_list!(comments_ws!(char!(',')), parse_data))) >>
	char!(';') >>
	(Property {deleted: false, name: name, val: data, labels: labels})
)));

named!(parse_node<Node>, comments_ws!(do_parse!(
	labels: many0!(terminated!(parse_label, char!(':'))) >>
	name: map!(map_res!(alt!(
		take_while1!(is_prop_node_char) |
		tag!("/")
	), str::from_utf8), String::from) >>
	char!('{') >>
	props: many0!(parse_prop) >>
	subnodes: many0!(parse_node) >>
	char!('}') >>
	char!(';') >>
	(Node { name: name, deleted: false, proplist: props, children: subnodes, labels: labels })
)));

named!(parse_ammend<Node>, comments_ws!(do_parse!(
	labels: many0!(terminated!(parse_label, char!(':'))) >>
	name: alt!(
		map!(map_res!(tag!("/"), str::from_utf8), String::from) |
		call!(parse_ref)
	) >>
	char!('{') >>
	props: many0!(parse_prop) >>
	subnodes: many0!(parse_node) >>
	char!('}') >>
	char!(';') >>
	(Node { name: name, deleted: false, proplist: props, children: subnodes, labels: labels })
)));

//TODO: stuff after main block
named!(parse_device_tree<Node>, comments_ws!(preceded!(peek!(char!('/')), parse_node)));

named!(parse_dts<(BootInfo, Vec<Node>)>, comments_ws!(do_parse!(
	tag!("/dts-v1/;") >>
	mem_reserves: many0!(parse_mem_reserve) >>
	device_tree: parse_device_tree >>
	ammendments: many0!(parse_ammend) >>
	(BootInfo { reserve_info: mem_reserves, root: device_tree, boot_cpuid: 0 }, ammendments)
)));

//TODO: imports
//TODO: delete nodes
//TODO: delete props
//TODO: error messages
pub fn parse_dt(source: &[u8]) -> Result<(BootInfo, Vec<Node>), String> {
	match parse_dts(source) {
		IResult::Done(remaining, device_tree) => {
			if remaining.len() > 0 {
				Err(format!("Remaining input after completion: {}", String::from_utf8_lossy(remaining)))
			} else {
				Ok(device_tree)
			}
		},
		IResult::Incomplete(_) => Err("Incomplete input".to_string()),
		IResult::Error(err) => {
			//println!("{:?}", );
			Err(format!("Error during parsing: {:?}", err))
		},
	}
}

//TODO: try not to eat whitespace past thing
named!(pub parse_include<String>, ws!(preceded!(
	tag!("/include/"),
	delimited!(
		char!('"'),
		do_parse!(
			val: escape_c_string >>
			(val)
		),
		char!('"')
	)
)));

#[cfg(test)]
mod tests {
	use super::*;
	use nom::IResult;

	#[test]
	fn parse_prop_empty() {
		assert_eq!(
			parse_prop(b"empty_prop;"), 
			IResult::Done(
				&b""[..],
				Property {
					deleted: false,
					name: "empty_prop".to_string(),
					val: None,
					labels: Vec::new(),
				}
			)
		);
	}

	#[test]
	fn parse_prop_cells() {
		assert_eq!(
			parse_prop(b"cell_prop = < 1 2 10 >;"), 
			IResult::Done(
				&b""[..],
				Property {
					deleted: false,
					name: "cell_prop".to_string(),
					val: Some(vec![Data::Cells(32, vec![(1, None), (2, None), (10, None)])]),
					labels: Vec::new(),
				}
			)
		);
	}

	#[test]
	fn parse_prop_strings() {
		assert_eq!(
			parse_prop(b"string_prop = \"string\", \"string2\";"), 
			IResult::Done(
				&b""[..],
				Property {
					deleted: false,
					name: "string_prop".to_string(),
					val: Some(vec![Data::String("string".to_string()), Data::String("string2".to_string())]),
					labels: Vec::new(),
				}
			)
		);
	}

	#[test]
	fn parse_prop_bytes() {
		assert_eq!(
			parse_prop(b"bytes_prop = [1234 56 78];"), 
			IResult::Done(
				&b""[..],
				Property {
					deleted: false,
					name: "bytes_prop".to_string(),
					val: Some(vec![Data::ByteArray(vec![0x12, 0x34, 0x56, 0x78])]),
					labels: Vec::new(),
				}
			)
		);
	}

	#[test]
	fn parse_prop_mixed() {
		assert_eq!(
			parse_prop(b"mixed_prop = \"abc\", [1234], <0xa 0xb 0xc>;"), 
			IResult::Done(
				&b""[..],
				Property {
					deleted: false,
					name: "mixed_prop".to_string(),
					val: Some(vec![
						Data::String("abc".to_string()),
						Data::ByteArray(vec![0x12, 0x34]),
						Data::Cells(32, vec![(0xa, None), (0xb, None), (0xc, None)])
					]),
					labels: Vec::new(),
				}
			)
		);
	}

	#[test]
	fn block_comment() {
		assert_eq!(
			parse_prop(b"test_prop /**/ = < 1 2 10 >;"), 
			IResult::Done(
				&b""[..],
				Property {
					deleted: false,
					name: "test_prop".to_string(),
					val: Some(vec![Data::Cells(32, vec![(1, None), (2, None), (10, None)])]),
					labels: Vec::new(),
				}
			)
		);
	}

	#[test]
	fn line_comment() {
		assert_eq!(
			parse_prop(b"test_prop // stuff\n\t= < 1 2 10 >;"), 
			IResult::Done(
				&b""[..],
				Property {
					deleted: false,
					name: "test_prop".to_string(),
					val: Some(vec![Data::Cells(32, vec![(1, None), (2, None), (10, None)])]),
					labels: Vec::new(),
				}
			)
		);
	}

	#[test]
	fn parse_data_string_pain() {
		assert_eq!(
			parse_data(b"\"\\x7f\\0stuffstuff\\t\\t\\t\\n\\n\\n\""), 
			IResult::Done(
				&b""[..],
				Data::String("\x7f\0stuffstuff\t\t\t\n\n\n".to_string())
			)
		);
	}

	#[test]
	fn parse_data_sized_cell_1() {
		assert_eq!(
			parse_data(b"/bits/ 8 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xde>"),
			IResult::Done(&b""[..], Data::Cells(8, vec![
				(b'\r' as u64, None), (b'b' as u64, None), (0, None),
				(b'\'' as u64, None), (0xFF, None), (0xDE, None)
			]))
		);
	}

	#[test]
	fn parse_data_sized_cell_2() {
		assert_eq!(
			parse_data(b"/bits/ 16 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdead>"),
			IResult::Done(&b""[..], Data::Cells(16, vec![
				(b'\r' as u64, None), (b'b' as u64, None), (0, None),
				(b'\'' as u64, None), (0xFF, None), (0xDEAD, None)
			]))
		);
	}

	#[test]
	fn parse_data_sized_cell_3() {
		assert_eq!(
			parse_data(b"/bits/ 32 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdeadbeef>"),
			IResult::Done(&b""[..], Data::Cells(32, vec![
				(b'\r' as u64, None), (b'b' as u64, None), (0, None),
				(b'\'' as u64, None), (0xFF, None), (0xDEADBEEF, None)
			]))
		);
	}

	#[test]
	fn parse_data_sized_cell_4() {
		assert_eq!(
			parse_data(b"/bits/ 64 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdeadbeef00000000>"),
			IResult::Done(&b""[..], Data::Cells(64, vec![
				(b'\r' as u64, None), (b'b' as u64, None), (0, None),
				(b'\'' as u64, None), (0xFF, None), (0xDEADBEEF00000000, None)
			]))
		);
	}

	#[test]
	fn parse_data_sized_cell_5() {
		assert_eq!(
			parse_data(b"<0x12345678 0x0000ffff>"),
			IResult::Done(&b""[..], Data::Cells(32, vec![(0x12345678, None), (0x0000FFFF, None)]))
		);
	}

	#[test]
	fn parse_data_sized_cell_6() {
		assert_eq!(
			parse_data(b"/bits/ 16 <0x1234 0x5678 0x0 0xffff>"),
			IResult::Done(&b""[..], Data::Cells(16,
				vec![(0x1234, None), (0x5678, None), (0, None), (0xFFFF, None)]
			))
		);
	}

	//TODO: incorrect sized cells
	//TODO: ref in non 32 bit cells

	#[test]
	fn parse_integer_1() {
		assert_eq!(
			parse_c_expr(b"(64)"),
			IResult::Done(&b""[..], 64)
		);
	}

	#[test]
	fn parse_integer_2() {
		assert_eq!(
			parse_c_expr(b"(1 << 5)"),
			IResult::Done(&b""[..], 32)
		);
	}

	#[test]
	fn parse_integer_3() {
		assert_eq!(
			parse_c_expr(b"(((1 << 5)) | 7)"),
			IResult::Done(&b""[..], 39)
		);
	}

	#[test]
	fn parse_integer_4() {
		assert_eq!(
			parse_c_expr(b"((((50))))"),
			IResult::Done(&b""[..], 50)
		);
	}

	#[test]
	fn parse_integer_5() {
		assert_eq!(
			parse_c_expr(b"((((0x910)) & 0xffff) - (0x800))"),
			IResult::Done(&b""[..], 272)
		);
	}

	#[test]
	fn parse_math_cell_1() {
		assert_eq!(
			parse_data(b"< ((((0x910)) & 0xffff) - (0x800)) >"),
			IResult::Done(&b""[..], Data::Cells(32, vec![(272, None)]))
		);
	}

	#[test]
	fn parse_math_cell_2() {
		assert_eq!(
			parse_data(b"< ((((0x910)) & 0xffff) - (0x800)) (0 | 3) >"),
			IResult::Done(&b""[..], Data::Cells(32, vec![(272, None), (3, None)]))
		);
	}
}
