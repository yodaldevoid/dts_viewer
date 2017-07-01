//! This module's main focus is `parse_dt` which parses a DTS file and returns
//! an unflattened device tree.
//!
//! In addition there are a couple of utility C-style escaped strings and
//! characters parsing functions.

#![allow(trivial_numeric_casts)]

use std::str::{self, FromStr};
use std::num::ParseIntError;

use nom::{IResult, ErrorKind, hex_digit, oct_digit, digit, is_alphanumeric, alpha, line_ending,
          not_line_ending, multispace, space, rest};

use tree::{BootInfo, ReserveInfo, Node, NodeName, Property, Data, Cell};
use ::ParseError;

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

fn from_str_hex<T: FromStrRadix>(s: &str) -> Result<T, T::Err> {
    T::from_str_radix(s, 16)
}

fn from_str_oct<T: FromStrRadix>(s: &str) -> Result<T, T::Err> {
    T::from_str_radix(s, 8)
}
fn from_str_dec<T: FromStr>(s: &str) -> Result<T, T::Err> {
    T::from_str(s)
}

// This is dumb and feels wrong, but it works so I will complain no more.
// Thank you to Filipe Gonçalves and https://crates.io/crates/config for the inspiration.
named!(eat_junk,
       do_parse!(many0!(alt!(delimited!(tag!("/*"), take_until!("*/"), tag!("*/")) |
                             delimited!(tag!("//"), not_line_ending, line_ending) |
                             do_parse!(tag!("#") >> opt!(tag!("line")) >> space >>
                                       digit >>
                                       space >>
                                       string: not_line_ending >>
                                       line_ending >>
                                       (string)) | multispace)) >> (&b""[..])));

macro_rules! comments_ws (
    ($i:expr, $($args:tt)*) => ( {
        use parser::eat_junk;
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
    Paren,
}

fn parse_c_expr(input: &[u8]) -> IResult<&[u8], u64> {
    let mut stack = Vec::new();
    // println!("Input: {:?}", String::from_utf8_lossy(input));
    let mut buf = input;
    loop {
        // println!("Stack: {:?}", stack);
        buf = if let IResult::Done(cleaned, _) = eat_junk(buf) {
            // println!("cleaned: {}", String::from_utf8_lossy(cleaned));
            if let IResult::Done(matched, num) = integer(cleaned) {
                match stack.pop() {
                    None => {
                        // println!("Pop Nothing");
                        stack.push(Token::Number(num));
                    }
                    Some(Token::Paren) => {
                        // println!("Pop Paren");
                        stack.push(Token::Paren);
                        stack.push(Token::Number(num));
                    }
                    Some(Token::Prefix(ref x)) => {
                        // println!("Pop Prefix");
                        let num = x.apply(num);
                        stack.push(Token::Number(num));
                    }
                    Some(Token::Infix(ref x)) => {
                        // println!("Pop Infix");
                        if let Some(Token::Number(a)) = stack.pop() {
                            let num = x.apply(a, num);
                            stack.push(Token::Number(num));
                        } else {
                            return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
                        }
                    }
                    Some(Token::Number(a)) if stack.is_empty() => return IResult::Done(buf, a),
                    _ => return IResult::Error(error_position!(ErrorKind::Custom(1), buf)),
                };

                matched
            } else if let IResult::Done(matched, tok) = opr_infix(cleaned) {
                // println!("Infix: {:?}", tok);
                if let Some(&Token::Number(a)) = stack.last() {
                    if (tok == OprInfix::Greater || tok == OprInfix::BitAnd) && stack.len() == 1 {
                        if let IResult::Done(cleaned, _) = eat_junk(matched) {
                            named!(test, preceded!(
                                eat_junk,
                                alt!(
                                    tag!("(") |
                                    recognize!(opr_prefix) |
                                    recognize!(integer)
                            )));
                            match test(cleaned) {
                                IResult::Done(_, _) => {}
                                _ => return IResult::Done(buf, a),
                            }
                        }
                    }
                    stack.push(Token::Infix(tok));
                } else if tok == OprInfix::Subtract {
                    stack.push(Token::Prefix(OprPrefix::Negate));
                } else {
                    return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
                };

                matched
            } else if let IResult::Done(matched, tok) = opr_prefix(cleaned) {
                // println!("Prefix: {:?}", tok);
                if let Some(&Token::Number(a)) = stack.last() {
                    if stack.len() == 1 {
                        return IResult::Done(buf, a);
                    }
                    return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
                } else {
                    stack.push(Token::Prefix(tok));
                };

                matched
            } else if let Some(&c) = cleaned.first() {
                match c {
                    b'(' => {
                        // println!("Paren");
                        match stack.pop() {
                            None => {
                                stack.push(Token::Paren);
                            }
                            Some(x @ Token::Paren) |
                            Some(x @ Token::Prefix(_)) |
                            Some(x @ Token::Infix(_)) => {
                                stack.push(x);
                                stack.push(Token::Paren);
                            }
                            Some(Token::Number(a)) => {
                                if stack.is_empty() {
                                    return IResult::Done(buf, a);
                                } else {
                                    return IResult::Error(
                                        error_position!(ErrorKind::Custom(1), buf)
                                    );
                                }
                            }
                        };
                    }

                    b')' => {
                        // println!("Close");
                        if let Some(Token::Number(num)) = stack.pop() {
                            if let Some(Token::Paren) = stack.pop() {
                                match stack.pop() {
                                    None => {
                                        stack.push(Token::Number(num));
                                    }
                                    Some(Token::Paren) => {
                                        stack.push(Token::Paren);
                                        stack.push(Token::Number(num));
                                    }
                                    Some(Token::Prefix(ref x)) => {
                                        let num = x.apply(num);
                                        stack.push(Token::Number(num));
                                    }
                                    Some(Token::Infix(ref x)) => {
                                        if let Some(Token::Number(a)) = stack.pop() {
                                            let num = x.apply(a, num);
                                            stack.push(Token::Number(num));
                                        } else {
                                            return IResult::Error(
                                                error_position!(ErrorKind::Custom(1), buf)
                                            );
                                        }
                                    }
                                    _ => return IResult::Error(
                                            error_position!(ErrorKind::Custom(1), buf)
                                        ),
                                };
                            } else {
                                return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
                            }
                        } else {
                            return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
                        }
                    }

                    b';' => {
                        match stack.pop() {
                            Some(Token::Number(a)) => {
                                if stack.is_empty() {
                                    return IResult::Done(buf, a);
                                } else {
                                    return IResult::Error(
                                        error_position!(ErrorKind::Custom(1), buf)
                                    );
                                }
                            }
                            _ => return IResult::Error(error_position!(ErrorKind::Custom(1), buf)),
                        }
                    }

                    x => {
                        println!("Unimplemented char: {}", x as char);
                        return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
                    }
                }

                &cleaned[1..]
            } else {
                // Incomplete
                println!("Incomplete: {}", String::from_utf8_lossy(buf));
                unreachable!()
            }
        } else {
            // println!("Junk eating failed!?");
            unreachable!()
        };

        if buf.is_empty() {
            if stack.len() == 1 {
                if let Some(&Token::Number(num)) = stack.last() {
                    return IResult::Done(buf, num);
                } else {
                    return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
                }
            } else {
                return IResult::Error(error_position!(ErrorKind::Custom(1), buf));
            }
        }
    }
}

// TODO: - issue 5
/*
comments_ws!(do_parse!( // trinary
    a: flat_map!(take_until_and_consume!("?"), integer) >>
    b: flat_map!(take_until_and_consume!(":"), integer) >>
    c: integer >>
    (if a != 0 { b } else { c })
)) |
*/

// ([0-9]+|0[xX][0-9a-fA-F]+)(U|L|UL|LL|ULL)
named!(integer<u64>, terminated!(
    alt_complete!(
        complete!(preceded!(tag_no_case!("0x"),
            map_res!(map_res!(hex_digit, str::from_utf8), from_str_hex::<u64>))) |
        preceded!(tag_no_case!("0"),
            map_res!(map_res!(oct_digit, str::from_utf8), from_str_oct::<u64>)) |
        map_res!(map_res!(digit, str::from_utf8), from_str_dec::<u64>)
    ),
    opt!(alt!(tag!("ULL") | tag!("LL") | tag!("UL") | tag!("L") | tag!("U")))
));

fn is_prop_node_char(c: u8) -> bool {
    is_alphanumeric(c) || c == b',' || c == b'.' || c == b'_' || c == b'+' || c == b'*' ||
    c == b'#' || c == b'?' || c == b'@' || c == b'-'
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

named!(transform<Vec<u8>>, escaped_transform!(take_until_either!("\\\""), '\\', alt!(
    tag!("a")   => { |_| vec![b'\x07'] } |
    tag!("b")   => { |_| vec![b'\x08'] } |
    tag!("t")   => { |_| vec![b'\t'] } |
    tag!("n")   => { |_| vec![b'\n'] } |
    tag!("v")   => { |_| vec![b'\x0B'] } |
    tag!("f")   => { |_| vec![b'\x0C'] } |
    tag!("r")   => { |_| vec![b'\r'] } |
    tag!("\\")  => { |_| vec![b'\\'] } |
    tag!("\"")  => { |_| vec![b'\"'] } |
    preceded!(
        tag_no_case!("x"),
        map_res!(map_res!(hex_digit, str::from_utf8), from_str_hex::<u8>)
    ) => { |c| vec![c] } |
    map_res!(map_res!(oct_digit, str::from_utf8), from_str_oct::<u8>) => { |c| vec![c] }
)));

named_attr!(#[doc =
"Parse a slice of bytes as a `String`, replacing escape codes with the
appropriate character. Characters may be described as a C-style escape code.
All hexadecimal and octal escape codes work up to 0x7f or x177,
respectively. This is due to the conversion to `char`s. See
`parser::escape_c_char` if a conversion of a character beyond this change is
needed."
], pub escape_c_string<String>, map_res!(
    alt!(
        call!(transform) |
        map!(verify!(take_until!("\""), |s: &[u8]| s.is_empty()), |_| vec![])
    ),
    String::from_utf8)
);

named_attr!(#[doc =
"Parse a slice of bytes as a ASCII character. The character may be described
as a C-style escape code. All hexadecimal and octal escape codes work up to
0xff or x777, respectively, because they are not converted to `char`s."
], pub escape_c_char<u8>, alt!(
    tag!("\\a")     => { |_| 0x07 } |
    tag!("\\b")     => { |_| 0x08 } |
    tag!("\\t")     => { |_| b'\t' } |
    tag!("\\n")     => { |_| b'\n' } |
    tag!("\\v")     => { |_| 0x0B } |
    tag!("\\f")     => { |_| 0x0C } |
    tag!("\\r")     => { |_| b'\r' } |
    tag!("\\\\")    => { |_| b'\\' } |
    tag!("\\\'")    => { |_| b'\'' } |
    preceded!(
        tag_no_case!("\\x"),
        map_res!(map_res!(hex_digit, str::from_utf8), from_str_hex::<u8>)
    ) |
    preceded!(tag!("\\"), map_res!(map_res!(oct_digit, str::from_utf8), from_str_oct::<u8>)) |
    take!(1) => { |c: &[u8]| c[0] }
));

named_args!(parse_cell(bits: usize)<Cell>, 
    alt!(
        map!(verify!(
            alt!(
                map!(delimited!(
                    char!('\''),
                    escape_c_char,
                    char!('\'')
                ), u64::from) |
                parse_c_expr
            ),
            |num| {
                if bits != 64 {
                    let mask = (1 << bits) - 1;
                    !((num > mask) && ((num | mask) != u64::max_value()))
                } else {
                    true
                }
            }
        ), |num| Cell::Num(num)) |
        map!(cond_reduce!(bits == 32, parse_ref), |s| Cell::Ref(s, None))
    )
);

named!(parse_mem_reserve<ReserveInfo>, comments_ws!(do_parse!(
    labels: many0!(terminated!(parse_label, char!(':'))) >>
    tag!("/memreserve/") >>
    addr: parse_c_expr >>
    size: parse_c_expr >>
    char!(';') >>
    (ReserveInfo { address: addr, size: size, labels: labels })
)));

named!(parse_data_cells<Data>, do_parse!(
    bits: verify!(
        map!(opt!(complete!(comments_ws!(preceded!(
            tag!("/bits/"),
            flat_map!(take_until!("<"), integer)
        )))), |b: Option<u64>| b.unwrap_or(32)),
    |b| b == 8 || b == 16 || b == 32 || b == 64 ) >>
    val: delimited!(
        comments_ws!(char!('<')),
            separated_list!(eat_junk, call!(parse_cell, bits as usize)),
        comments_ws!(char!('>'))
    ) >>
    ( Data::Cells(bits as usize, val) )
));

// TODO: labels in data - issue 6
// TODO: include binary - issue 7
named!(parse_data<Data>, comments_ws!(alt!(
    delimited!(
        char!('"'),
        map!(escape_c_string, |s| Data::String(s)),
        char!('"')
    ) |
    call!(parse_data_cells) |
    delimited!(
        char!('['),
        do_parse!(
            val: many1!(map_res!(map_res!(
                    comments_ws!(take!(2)), str::from_utf8), from_str_hex::<u8>)) >>
            (Data::ByteArray(val))
        ),
        char!(']')
    ) |
    map!(parse_ref, |x| (Data::Reference(x, None)))
)));

named_args!(parse_prop(input_len: usize)<Property>, comments_ws!(alt!(
    do_parse!(
        offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
        tag!("/delete-property/") >>
        name: map!(map_res!(take_while1!(is_prop_node_char), str::from_utf8), String::from) >>
        char!(';') >>
        ( Property::Deleted { name: name, offset: input_len - offset } )
    ) |
    do_parse!(
        offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
        labels: many0!(terminated!(parse_label, char!(':'))) >>
        name: map!(map_res!(take_while1!(is_prop_node_char), str::from_utf8), String::from) >>
        data: opt!(preceded!(
            char!('='),
            separated_nonempty_list!(comments_ws!(char!(',')), parse_data))) >>
        char!(';') >>
        ( Property::Existing { name: name,
                               val: data,
                               labels: labels,
                               offset: input_len - offset } )
    )
)));

named_args!(parse_node(input_len: usize)<Node>, comments_ws!(alt!(
    do_parse!(
        offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
        tag!("/delete-node/") >>
        name: map!(map_res!(take_while1!(is_prop_node_char), str::from_utf8), String::from) >>
        char!(';') >>
        ( Node::Deleted { name: NodeName::Full(name), offset: input_len - offset } )
    ) |
    do_parse!(
        offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
        labels: many0!(terminated!(parse_label, char!(':'))) >>
        name: map!(map_res!(alt!(
            take_while1!(is_prop_node_char) |
            tag!("/")
        ), str::from_utf8), String::from) >>
        char!('{') >>
        props: many0!(call!(parse_prop, input_len)) >>
        subnodes: many0!(call!(parse_node, input_len)) >>
        char!('}') >>
        char!(';') >>
        ( Node::Existing { name: NodeName::Full(name),
                           proplist: props.into_iter()
                                          .map(|p| (p.name().to_owned(), p))
                                          .collect(),
                           children: subnodes.into_iter()
                                             .map(|n| (n.name().as_str().to_owned(), n))
                                             .collect(),
                           labels: labels,
                           offset: input_len - offset } )
    )
)));

named_args!(parse_amend(input_len: usize)<Node>, comments_ws!(alt!(
    do_parse!(
        offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
        tag!("/delete-node/") >>
        name: parse_ref >>
        char!(';') >>
        ( Node::Deleted { name: NodeName::Ref(name), offset: input_len - offset } )
    ) |
    do_parse!(
        offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
        labels: many0!(terminated!(parse_label, char!(':'))) >>
        name: alt!(
            map!(map!(map_res!(tag!("/"), str::from_utf8), String::from), |x| NodeName::Full(x)) |
            map!(parse_ref, |x| NodeName::Ref(x))
        ) >>
        char!('{') >>
        props: many0!(call!(parse_prop, input_len)) >>
        subnodes: many0!(call!(parse_node, input_len)) >>
        char!('}') >>
        char!(';') >>
        ( Node::Existing { name: name,
                           proplist: props.into_iter()
                                          .map(|p| (p.name().to_owned(), p))
                                          .collect(),
                           children: subnodes.into_iter()
                                             .map(|n| (n.name().as_str().to_owned(), n))
                                             .collect(),
                           labels: labels,
                           offset: input_len - offset } )
    )
)));

named_args!(parse_device_tree(input_len: usize)<Node>,
            comments_ws!(preceded!(peek!(char!('/')), call!(parse_node, input_len))));

named_args!(parse_dts(input_len: usize)<(BootInfo, Vec<Node>)>, comments_ws!(do_parse!(
    tag!("/dts-v1/;") >>
    mem_reserves: many0!(parse_mem_reserve) >>
    device_tree: call!(parse_device_tree, input_len) >>
    amendments: many0!(call!(parse_amend, input_len)) >>
    // TODO: set boot cpu id - issue 8
    (BootInfo { reserve_info: mem_reserves, root: device_tree, boot_cpuid: 0 }, amendments)
)));

/// Returned on a successful completion of `parse_dt`.
#[derive(Debug)]
pub enum ParseResult<'a> {
    /// Indicates that the entirety of the buffer was used while parsing. Holds
    /// the boot info that includes the first root node and a `Vec` of all
    /// following nodes.
    Complete(BootInfo, Vec<Node>),
    /// Indicates that only of the buffer was used while parsing. Holds the boot
    /// info that includes the first root node, a `Vec` of all following nodes,
    /// and a slice containing the remainder of the buffer. Having left over
    /// output after parsing is generally not expected and in most cases should
    /// be considered an error.
    RemainingInput(BootInfo, Vec<Node>, &'a [u8])
}

/// Parses the slice of `u8`s as ASCII characters and returns a device tree made
/// of the first root node and a `Vec` of nodes defined after that. The nodes
/// defined after the first root node may specify a node by label to modify or
/// my start at the root node. These amendments to the root node can be merged
/// into the device tree manually or by `tree::apply_amends`.
///
/// When a tree and any following nodes are parsed successfully without
/// remaining input `ParseResult::Complete` is returned containing the tree and
/// the following nodes. If there is remaining input
/// `ParseResult::RemainingInput` is returned with the tree, following nodes,
/// and a slice of the remaining input.
///
/// # Errors
/// Returns `ParseError::IncompleteInput` if the end of the input was reached
/// where more was expected.
/// Returns `ParseError::NomError` if a `nom` parsing error was returned. This
/// doesn't help much right now, but will be improved soon.
pub fn parse_dt(source: &[u8]) -> Result<ParseResult, ParseError> {
    match parse_dts(source, source.len()) {
        IResult::Done(remaining, (tree, amends)) => {
            if remaining.is_empty() {
                Ok(ParseResult::Complete(tree, amends))
            } else {
                Ok(ParseResult::RemainingInput(tree, amends, remaining))
            }
        }
        IResult::Incomplete(_) => Err(ParseError::IncompleteInput),
        IResult::Error(_err) => {
            // TODO: specific error messages - issue 4
            // println!("{:?}", _err);
            Err(ParseError::NomError)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use nom::IResult;

    #[test]
    fn prop_empty() {
        let input = b"empty_prop;";
        assert_eq!(
            parse_prop(input, input.len()),
            IResult::Done(
                &b""[..],
                Property::Existing {
                    name: "empty_prop".to_owned(),
                    val: None,
                    labels: Vec::new(),
                    offset: 0,
                }
            )
        );
    }

    #[test]
    fn prop_cells() {
        let input = b"cell_prop = < 1 2 10 >;";
        assert_eq!(
            parse_prop(input, input.len()),
            IResult::Done(
                &b""[..],
                Property::Existing {
                    name: "cell_prop".to_owned(),
                    val: Some(vec![Data::Cells(32, vec![Cell::Num(1), Cell::Num(2), Cell::Num(10)])]),
                    labels: Vec::new(),
                    offset: 0,
                }
            )
        );
    }

    #[test]
    fn prop_strings() {
        let input = b"string_prop = \"string\", \"string2\";";
        assert_eq!(
            parse_prop(input, input.len()),
            IResult::Done(
                &b""[..],
                Property::Existing {
                    name: "string_prop".to_owned(),
                    val: Some(vec![
                            Data::String("string".to_owned()),
                            Data::String("string2".to_owned())
                         ]),
                    labels: Vec::new(),
                    offset: 0,
                }
            )
        );
    }

    #[test]
    fn prop_bytes() {
        let input = b"bytes_prop = [1234 56 78];";
        assert_eq!(
            parse_prop(input, input.len()),
            IResult::Done(
                &b""[..],
                Property::Existing {
                    name: "bytes_prop".to_owned(),
                    val: Some(vec![Data::ByteArray(vec![0x12, 0x34, 0x56, 0x78])]),
                    labels: Vec::new(),
                    offset: 0,
                }
            )
        );
    }

    #[test]
    fn prop_mixed() {
        let input = b"mixed_prop = \"abc\", [1234], <0xa 0xb 0xc>;";
        assert_eq!(
            parse_prop(input, input.len()),
            IResult::Done(
                &b""[..],
                Property::Existing {
                    name: "mixed_prop".to_owned(),
                    val: Some(vec![
                        Data::String("abc".to_owned()),
                        Data::ByteArray(vec![0x12, 0x34]),
                        Data::Cells(32, vec![Cell::Num(0xa), Cell::Num(0xb), Cell::Num(0xc)])
                    ]),
                    labels: Vec::new(),
                    offset: 0,
                }
            )
        );
    }

    #[test]
    fn block_comment() {
        let input = b"test_prop /**/ = < 1 2 10 >;";
        assert_eq!(
            parse_prop(input, input.len()),
            IResult::Done(
                &b""[..],
                Property::Existing {
                    name: "test_prop".to_owned(),
                    val: Some(vec![Data::Cells(32, vec![Cell::Num(1), Cell::Num(2), Cell::Num(10)])]),
                    labels: Vec::new(),
                    offset: 0,
                }
            )
        );
    }

    #[test]
    fn line_comment() {
        let input = b"test_prop // stuff\n\t= < 1 2 10 >;";
        assert_eq!(
            parse_prop(input, input.len()),
            IResult::Done(
                &b""[..],
                Property::Existing {
                    name: "test_prop".to_owned(),
                    val: Some(vec![Data::Cells(32, vec![Cell::Num(1), Cell::Num(2), Cell::Num(10)])]),
                    labels: Vec::new(),
                    offset: 0,
                }
            )
        );
    }

    #[test]
    fn data_string_pain() {
        assert_eq!(
            parse_data(b"\"\\x7f\\0stuffstuff\\t\\t\\t\\n\\n\\n\""),
            IResult::Done(
                &b""[..],
                Data::String("\x7f\0stuffstuff\t\t\t\n\n\n".to_owned())
            )
        );
    }

    #[test]
    fn data_string_empty() {
        assert_eq!(
            parse_data(b"\"\""),
            IResult::Done(
                &b""[..],
                Data::String("".to_owned())
            )
        );
    }

    #[test]
    fn data_cell_sized_8_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 8 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xde>"),
            IResult::Done(&b""[..], Data::Cells(8, vec![
                Cell::Num(b'\r' as u64), Cell::Num(b'b' as u64), Cell::Num(0),
                Cell::Num(b'\'' as u64), Cell::Num(0xFF), Cell::Num(0xDE)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_16_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 16 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdead>"),
            IResult::Done(&b""[..], Data::Cells(16, vec![
                Cell::Num(b'\r' as u64), Cell::Num(b'b' as u64), Cell::Num(0),
                Cell::Num(b'\'' as u64), Cell::Num(0xFF), Cell::Num(0xDEAD)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_32_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 32 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdeadbeef>"),
            IResult::Done(&b""[..], Data::Cells(32, vec![
                Cell::Num(b'\r' as u64), Cell::Num(b'b' as u64), Cell::Num(0),
                Cell::Num(b'\'' as u64), Cell::Num(0xFF), Cell::Num(0xDEADBEEF)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_64_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 64 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdeadbeef00000000>"),
            IResult::Done(&b""[..], Data::Cells(64, vec![
                Cell::Num(b'\r' as u64), Cell::Num(b'b' as u64), Cell::Num(0),
                Cell::Num(b'\'' as u64), Cell::Num(0xFF), Cell::Num(0xDEADBEEF00000000)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_default() {
        assert_eq!(
            parse_data(b"<0x12345678 0x0000ffff>"),
            IResult::Done(&b""[..], Data::Cells(32, vec![Cell::Num(0x12345678), Cell::Num(0x0000FFFF)]))
        );
    }

    #[test]
    fn data_cell_sized_16() {
        assert_eq!(
            parse_data(b"/bits/ 16 <0x1234 0x5678 0x0 0xffff>"),
            IResult::Done(&b""[..], Data::Cells(16,
                vec![Cell::Num(0x1234), Cell::Num(0x5678), Cell::Num(0), Cell::Num(0xFFFF)]
            ))
        );
    }

    #[test]
    fn data_cell_sized_incorrect() {
        match parse_data(b"/bits/ 16 <0x12345678 0x0000ffff>") {
            IResult::Error(_) => {},
            x => panic!(format!("parse_data did not return error: {:?}", x)),
        }
    }

    #[test]
    fn data_cell_sized_incorrect_ref() {
        match parse_data(b"/bits/ 16 <&ref>") {
            IResult::Error(_) => {},
            x => panic!(format!("parse_data did not return error: {:?}", x)),
        }
    }

    #[test]
    fn data_cell_ref() {
        assert_eq!(
            parse_data(b"<&ref>"),
            IResult::Done(&b""[..], Data::Cells(32, vec![Cell::Ref("ref".to_owned(), None)]))
        );
    }

    #[test]
    fn data_cell_empty() {
        assert_eq!(
            parse_data(b"<>"),
            IResult::Done(&b""[..], Data::Cells(32, Vec::new()))
        );
    }

    #[test]
    fn integer_1() {
        assert_eq!(
            parse_c_expr(b"(64)"),
            IResult::Done(&b""[..], 64)
        );
    }

    #[test]
    fn integer_2() {
        assert_eq!(
            parse_c_expr(b"(1 << 5)"),
            IResult::Done(&b""[..], 32)
        );
    }

    #[test]
    fn integer_3() {
        assert_eq!(
            parse_c_expr(b"(((1 << 5)) | 7)"),
            IResult::Done(&b""[..], 39)
        );
    }

    #[test]
    fn integer_4() {
        assert_eq!(
            parse_c_expr(b"((((50))))"),
            IResult::Done(&b""[..], 50)
        );
    }

    #[test]
    fn integer_5() {
        assert_eq!(
            parse_c_expr(b"((((0x910)) & 0xffff) - (0x800))"),
            IResult::Done(&b""[..], 272)
        );
    }

    #[test]
    fn integer_6() {
        assert_eq!(
            parse_c_expr(b"(-1UL)"),
            IResult::Done(&b""[..], u64::max_value())
        );
    }

    #[test]
    fn math_cell_1() {
        assert_eq!(
            parse_data(b"< ((((0x910)) & 0xffff) - (0x800)) >"),
            IResult::Done(&b""[..], Data::Cells(32, vec![Cell::Num(272)]))
        );
    }

    #[test]
    fn math_cell_2() {
        assert_eq!(
            parse_data(b"< ((((0x910)) & 0xffff) - (0x800)) (0 | 3) >"),
            IResult::Done(&b""[..], Data::Cells(32, vec![Cell::Num(272), Cell::Num(3)]))
        );
    }
}
