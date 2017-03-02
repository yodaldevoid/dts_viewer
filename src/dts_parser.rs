use std::str::{self, FromStr};
use std::num::ParseIntError;
use std::fmt;

use nom::{IResult, ErrorKind, hex_digit, oct_digit, digit, is_alphanumeric, alpha, line_ending,
          not_line_ending, multispace, space, rest};

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

pub trait Offset {
    fn get_offset(&self) -> usize;
}

#[derive(Debug)]
pub struct BootInfo {
    pub reserve_info: Vec<ReserveInfo>,
    pub boot_cpuid: u32,
    pub root: Node,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Element<'a> {
    Node(&'a Node),
    Prop(&'a Property),
}

impl<'a> Offset for Element<'a> {
    fn get_offset(&self) -> usize {
        match *self {
            Element::Node(n) => n.get_offset(),
            Element::Prop(p) => p.get_offset(),
        }
    }
}

impl<'a> fmt::Display for Element<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Element::Node(node) => write!(f, "{}", node),
            Element::Prop(prop) => write!(f, "{}", prop),
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Node {
    Deleted { name: String, offset: usize },
    Existing {
        name: NodeName,
        proplist: Vec<Property>,
        children: Vec<Node>,

        // fullpath: Option<PathBuf>,
        // length to the # part of node_name@#
        // basenamelen: usize,
        //
        // phandle: u32,
        // addr_cells: i32,
        // size_cells: i32,
        labels: Vec<String>,

        offset: usize,
    },
}

impl Labeled for Node {
    fn add_label(&mut self, label: &str) {
        match *self {
            Node::Deleted { .. } => panic!("Why are you adding a label to a deleted node?!"),
            Node::Existing { ref mut labels, .. } => {
                let label = label.to_string();
                if labels.contains(&label) {
                    labels.push(label);
                }
            }
        }
    }
}

impl Offset for Node {
    fn get_offset(&self) -> usize {
        match *self {
            Node::Deleted { offset, .. } |
            Node::Existing { offset, .. } => offset,
        }
    }
}

impl fmt::Display for Node {
    // TODO: labels
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Node::Deleted { ref name, .. } => write!(f, "// Node {} deleted", name)?,
            Node::Existing { ref name, ref proplist, ref children, .. } => {
                writeln!(f, "{} {{", name)?;
                for prop in proplist {
                    writeln!(f, "    {}", prop)?;
                }
                for node in children {
                    match *node {
                        Node::Deleted { ref name, .. } => {
                            writeln!(f, "    // Node {} deleted", name)?
                        }
                        Node::Existing { ref name, .. } => writeln!(f, "    {} {{ ... }}", name)?,
                    }
                }
                write!(f, "}}")?;
            }
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum NodeName {
    Label(String),
    Full(String),
}

impl fmt::Display for NodeName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            NodeName::Label(ref name) |
            NodeName::Full(ref name) => write!(f, "{}", name),
        }
    }
}

impl NodeName {
    pub fn to_str(&self) -> &str {
        match *self {
            NodeName::Label(ref name) |
            NodeName::Full(ref name) => name,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Property {
    Deleted { name: String, offset: usize },
    Existing {
        name: String,
        val: Option<Vec<Data>>,
        labels: Vec<String>,
        offset: usize,
    },
}

impl Labeled for Property {
    fn add_label(&mut self, label: &str) {
        match *self {
            Property::Deleted { .. } => {
                panic!("Why are you adding a label to a deleted property?!")
            }
            Property::Existing { ref mut labels, .. } => {
                let label = label.to_string();
                if labels.contains(&label) {
                    labels.push(label);
                }
            }
        }
    }
}

impl Offset for Property {
    fn get_offset(&self) -> usize {
        match *self {
            Property::Deleted { offset, .. } |
            Property::Existing { offset, .. } => offset,
        }
    }
}

impl fmt::Display for Property {
    // TODO: labels
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Property::Deleted { ref name, .. } => write!(f, "// Property {} deleted", name)?,
            Property::Existing { ref name, ref val, .. } => {
                write!(f, "{}", name)?;
                if let Some(ref data) = *val {
                    if !data.is_empty() {
                        let mut iter = data.iter();
                        write!(f, " = {}", iter.next().unwrap())?;
                        for d in iter {
                            write!(f, ", {}", d)?;
                        }
                    }
                }
                write!(f, ";")?;
            }
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum Data {
    Reference(String),
    String(String),
    Cells(usize, Vec<(u64, Option<String>)>),
    ByteArray(Vec<u8>),
}

impl fmt::Display for Data {
    // TODO: labels
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Data::Reference(ref r) => write!(f, "&{}", r)?,
            Data::String(ref s) => write!(f, "{}", s)?,
            Data::Cells(bits, ref cells) => {
                if bits != 32 {
                    write!(f, "/bits/ {}", bits)?;
                }
                write!(f, "<")?;
                if !cells.is_empty() {
                    let mut iter = cells.iter();
                    match *iter.next().unwrap() {
                        (_, Some(ref s)) => write!(f, "&{}", s)?,
                        (i, None) => write!(f, "{}", i)?,
                    }
                    for d in iter {
                        match *d {
                            (_, Some(ref s)) => write!(f, ", &{}", s)?,
                            (i, None) => write!(f, ", {}", i)?,
                        }
                    }
                }
                write!(f, ">")?;
            }
            Data::ByteArray(ref arr) => {
                write!(f, "[ ")?;
                if !arr.is_empty() {
                    let mut iter = arr.iter();
                    write!(f, "{:02X}", iter.next().unwrap())?;
                    for d in iter {
                        write!(f, " {:02X}", d)?;
                    }
                }
                write!(f, " ]")?;
            }
        }

        Ok(())
    }
}

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
                             do_parse!(tag!("#") >> opt!(tag!("line")) >> many1!(space) >>
                                       many1!(digit) >>
                                       many1!(space) >>
                                       string: not_line_ending >>
                                       line_ending >>
                                       (string)) | multispace)) >> (&b""[..])));

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
    Paren,
}

pub fn parse_c_expr(input: &[u8]) -> IResult<&[u8], u64> {
    let mut stack = Vec::new();
    // println!("Input: {:?}", String::from_utf8_lossy(&input[..20]));
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
                            let test = closure!(preceded!(
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

        if buf.len() == 0 {
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

named!(pub integer<u64>, alt_complete!(
    // TODO:
    /*
    comments_ws!(do_parse!( // trinary
        a: flat_map!(take_until_and_consume!("?"), integer) >>
        b: flat_map!(take_until_and_consume!(":"), integer) >>
        c: integer >>
        (if a != 0 { b } else { c })
    )) |
    */
    complete!(preceded!(tag_no_case!("0x"),
        map_res!(map_res!(hex_digit, str::from_utf8), from_str_hex::<u64>))) |
    preceded!(tag_no_case!("0"),
        map_res!(map_res!(oct_digit, str::from_utf8), from_str_oct::<u64>)) |
    map_res!(map_res!(digit, str::from_utf8), from_str_dec::<u64>)
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

// Warning! Only supports hex escape codes up to 0x7f because UTF-8 reasons
named!(escape_c_string<String>, map_res!(
    escaped_transform!(take_until_either!("\\\""), '\\',
        alt!(
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
        )),
    String::from_utf8)
);

// Can support hex escape codes up to 0xff because we are not converting to char
named!(escape_c_char<u8>, alt!(
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
        if let IResult::Done(_, (num, _)) = result {
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

// TODO: labels in data
// TODO: include binary
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
            val: many1!(map_res!(map_res!(
                    comments_ws!(take!(2)), str::from_utf8), from_str_hex::<u8>)) >>
            (Data::ByteArray(val))
        ),
        char!(']')
    ) |
    do_parse!(val: parse_ref >> (Data::Reference(val)))
)));

named_args!(pub parse_prop(input_len: usize)<Property>, comments_ws!(alt!(
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
        ( Node::Deleted { name: name, offset: input_len - offset } )
    ) |
    do_parse!(
        offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
        labels: many0!(terminated!(parse_label, char!(':'))) >>
        name: map!(map_res!(alt!(
            take_while1!(is_prop_node_char) |
            tag!("/")
        ), str::from_utf8), String::from) >>
        char!('{') >>
        props: many0!(apply!(parse_prop, input_len)) >>
        subnodes: many0!(apply!(parse_node, input_len)) >>
        char!('}') >>
        char!(';') >>
        ( Node::Existing { name: NodeName::Full(name),
                           proplist: props,
                           children: subnodes,
                           labels: labels,
                           offset: input_len - offset } )
    )
)));

named_args!(parse_ammend(input_len: usize)<Node>, comments_ws!(do_parse!(
    offset: map!(peek!(rest), |x: &[u8]| x.len()) >>
    labels: many0!(terminated!(parse_label, char!(':'))) >>
    name: alt!(
        map!(map!(map_res!(tag!("/"), str::from_utf8), String::from), |x| NodeName::Full(x)) |
        map!(call!(parse_ref), |x| NodeName::Label(x))
    ) >>
    char!('{') >>
    props: many0!(apply!(parse_prop, input_len)) >>
    subnodes: many0!(apply!(parse_node, input_len)) >>
    char!('}') >>
    char!(';') >>
    ( Node::Existing { name: name,
                       proplist: props,
                       children: subnodes,
                       labels: labels,
                       offset: input_len - offset } )
)));

named_args!(parse_device_tree(input_len: usize)<Node>,
            comments_ws!(preceded!(peek!(char!('/')), apply!(parse_node, input_len))));

named_args!(parse_dts(input_len: usize)<(BootInfo, Vec<Node>)>, comments_ws!(do_parse!(
    tag!("/dts-v1/;") >>
    mem_reserves: many0!(parse_mem_reserve) >>
    device_tree: apply!(parse_device_tree, input_len) >>
    ammendments: many0!(apply!(parse_ammend, input_len)) >>
    (BootInfo { reserve_info: mem_reserves, root: device_tree, boot_cpuid: 0 }, ammendments)
)));

// TODO: error messages
pub fn parse_dt(source: &[u8]) -> Result<(BootInfo, Vec<Node>), String> {
    match parse_dts(source, source.len()) {
        IResult::Done(remaining, device_tree) => {
            if remaining.is_empty() {
                Ok(device_tree)
            } else {
                Err(format!(
                    "Remaining input after completion: {}",
                    String::from_utf8_lossy(remaining)
                ))
            }
        }
        IResult::Incomplete(_) => Err("Incomplete input".to_string()),
        IResult::Error(err) => Err(format!("Error during parsing: {:?}", err)),
    }
}

named!(pub parse_include<String>, preceded!(
    tag!("/include/"),
    preceded!( multispace,
        delimited!(
            char!('"'),
            do_parse!(
                val: escape_c_string >>
                (val)
            ),
            char!('"')
        ))
));

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
                    name: "empty_prop".to_string(),
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
                    name: "cell_prop".to_string(),
                    val: Some(vec![Data::Cells(32, vec![(1, None), (2, None), (10, None)])]),
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
                    name: "string_prop".to_string(),
                    val: Some(vec![
                            Data::String("string".to_string()),
                            Data::String("string2".to_string())
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
                    name: "bytes_prop".to_string(),
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
                    name: "mixed_prop".to_string(),
                    val: Some(vec![
                        Data::String("abc".to_string()),
                        Data::ByteArray(vec![0x12, 0x34]),
                        Data::Cells(32, vec![(0xa, None), (0xb, None), (0xc, None)])
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
                    name: "test_prop".to_string(),
                    val: Some(vec![Data::Cells(32, vec![(1, None), (2, None), (10, None)])]),
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
                    name: "test_prop".to_string(),
                    val: Some(vec![Data::Cells(32, vec![(1, None), (2, None), (10, None)])]),
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
                Data::String("\x7f\0stuffstuff\t\t\t\n\n\n".to_string())
            )
        );
    }

    #[test]
    fn data_cell_sized_8_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 8 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xde>"),
            IResult::Done(&b""[..], Data::Cells(8, vec![
                (b'\r' as u64, None), (b'b' as u64, None), (0, None),
                (b'\'' as u64, None), (0xFF, None), (0xDE, None)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_16_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 16 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdead>"),
            IResult::Done(&b""[..], Data::Cells(16, vec![
                (b'\r' as u64, None), (b'b' as u64, None), (0, None),
                (b'\'' as u64, None), (0xFF, None), (0xDEAD, None)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_32_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 32 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdeadbeef>"),
            IResult::Done(&b""[..], Data::Cells(32, vec![
                (b'\r' as u64, None), (b'b' as u64, None), (0, None),
                (b'\'' as u64, None), (0xFF, None), (0xDEADBEEF, None)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_64_escapes() {
        assert_eq!(
            parse_data(b"/bits/ 64 <'\\r' 'b' '\\0' '\\'' '\\xff' 0xdeadbeef00000000>"),
            IResult::Done(&b""[..], Data::Cells(64, vec![
                (b'\r' as u64, None), (b'b' as u64, None), (0, None),
                (b'\'' as u64, None), (0xFF, None), (0xDEADBEEF00000000, None)
            ]))
        );
    }

    #[test]
    fn data_cell_sized_default() {
        assert_eq!(
            parse_data(b"<0x12345678 0x0000ffff>"),
            IResult::Done(&b""[..], Data::Cells(32, vec![(0x12345678, None), (0x0000FFFF, None)]))
        );
    }

    #[test]
    fn data_cell_sized_16() {
        assert_eq!(
            parse_data(b"/bits/ 16 <0x1234 0x5678 0x0 0xffff>"),
            IResult::Done(&b""[..], Data::Cells(16,
                vec![(0x1234, None), (0x5678, None), (0, None), (0xFFFF, None)]
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
            IResult::Done(&b""[..], Data::Cells(32, vec![(0, Some("ref".to_string()))]))
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
    fn math_cell_1() {
        assert_eq!(
            parse_data(b"< ((((0x910)) & 0xffff) - (0x800)) >"),
            IResult::Done(&b""[..], Data::Cells(32, vec![(272, None)]))
        );
    }

    #[test]
    fn math_cell_2() {
        assert_eq!(
            parse_data(b"< ((((0x910)) & 0xffff) - (0x800)) (0 | 3) >"),
            IResult::Done(&b""[..], Data::Cells(32, vec![(272, None), (3, None)]))
        );
    }
}
