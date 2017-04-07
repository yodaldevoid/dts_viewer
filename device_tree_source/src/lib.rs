#[macro_use]
extern crate nom;

pub mod tree;
pub mod parser;
pub mod include;

use std::borrow::Borrow;

pub fn line_to_byte_offset<K, I>(bytes: I, line: usize) -> Result<usize, String>
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
            .ok_or_else(|| "Failed converting from line to byte offset".to_string())
    }
}

pub fn byte_offset_to_line_col<K, I>(bytes: I, offset: usize) -> (usize, usize)
    where K: Borrow<u8> + Eq,
          I: Iterator<Item = K>
{
    let opt = bytes.enumerate()
        .filter(|&(off, ref byte)| off < offset && byte.borrow() == &b'\n')
        .map(|(start, _)| start)
        .enumerate()
        .last();

    match opt {
        Some((line, start)) => (line + 2, offset - start),
        None => (1, offset + 1),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lines_to_bytes() {
        let string = "Howdy\nHow goes it\n\nI'm doing fine\n";
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 1).unwrap(),
                   0);
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 2).unwrap(),
                   6);
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 3).unwrap(),
                   18);
        assert_eq!(line_to_byte_offset(string.as_bytes().iter(), 4).unwrap(),
                   19);
    }

    #[test]
    fn bytes_to_lines() {
        let string = "Howdy\nHow goes it\n\nI'm doing fine\n";
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 0),
                   (1, 1));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 8),
                   (2, 3));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 20),
                   (4, 2));
        assert_eq!(byte_offset_to_line_col(string.as_bytes().iter(), 18),
                   (3, 1));
    }
}
