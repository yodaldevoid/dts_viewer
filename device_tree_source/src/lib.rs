#[macro_use]
extern crate nom;

pub mod tree;
pub mod parser;
pub mod include;

use std::borrow::Borrow;
use std::iter::once;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    NotFound,
}

/// Returns the byte offset of the starting character of line within the iterator.
///
/// Lines are assumed to be 1 indexed, and offsets are 0 indexed.
///
/// # Errors
/// Will return an `ParseError::NotFound` if the line cannot be found.
///
/// # Example
/// ```rust
/// use device_tree_source::line_to_byte_offset;
/// use device_tree_source::ParseError;
///
/// let string = "Howdy\nHow goes it\n\nI'm doing fine";
///
/// assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 1), Ok(0));
/// assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 3), Ok(18));
/// assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 5), Err(ParseError::NotFound));
/// ```
pub fn line_to_byte_offset<K, I>(bytes: I, line: usize) -> Result<usize, ParseError>
    where K: Borrow<u8> + Eq,
          I: Iterator<Item = K>
{
    if line == 1 {
        Ok(0)
    } else {
        bytes.enumerate()
            .filter(|&(_, ref byte)| byte.borrow() == &b'\n')
            .nth(line - 2)
            .map(|(offset, _)| offset + 1)
            .ok_or(ParseError::NotFound)
    }
}

/// Returns the line and column of the character at the offset within the iterator.
///
/// Offsets are assumed to be 0 indexed, and lines and columns are 1 indexed.
///
/// # Errors
/// Will return an `ParseError::NotFound` if the offset is past the end of the iterator.
///
/// # Example
/// ```rust
/// use device_tree_source::byte_offset_to_line_col;
/// use device_tree_source::ParseError;
///
/// let string = "Howdy\nHow goes it\n\nI'm doing fine";
///
/// assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 0), Ok((1, 1)));
/// assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 8), Ok((2, 3)));
/// assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 33), Err(ParseError::NotFound));
/// ```
pub fn byte_offset_to_line_col<K, I>(bytes: I, offset: usize) -> Result<(usize, usize), ParseError>
    where K: Borrow<u8> + Eq,
          I: Iterator<Item = K>
{
    let opt = bytes.map(|byte| Some(byte))
        .chain(once(None))
        .enumerate()
        .filter_map(|(off, byte)| match byte {
            Some(ref byte) if off < offset && byte.borrow() == &b'\n' => Some(Some(off)),
            None if off <= offset => Some(None),
            _ => None,
        })
        .enumerate()
        .last();

    match opt {
        Some((line, Some(start))) => Ok((line + 2, offset - start)),
        None => Ok((1, offset + 1)),
        Some((_, None)) => Err(ParseError::NotFound),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lines_to_bytes() {
        let string = "Howdy\nHow goes it\n\nI'm doing fine";
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 1), Ok(0));
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 2), Ok(6));
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 3), Ok(18));
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 4), Ok(19));
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 5), Err(ParseError::NotFound));
    }

    #[test]
    fn bytes_to_lines() {
        let string = "Howdy\nHow goes it\n\nI'm doing fine";
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 0), Ok((1, 1)));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 8), Ok((2, 3)));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 20), Ok((4, 2)));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 18), Ok((3, 1)));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 33),
                   Err(ParseError::NotFound));
    }
}
