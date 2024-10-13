use std::io::{Read, Write};

pub(crate) fn encode_leb128(f: &mut impl Write, value: u32) -> std::io::Result<()> {
    let mut value = value as u32;
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        f.write_all(&[byte])?;
        if value == 0 {
            return Ok(());
        }
    }
}

#[test]
fn test_leb128() {
    let mut v = vec![];
    encode_leb128(&mut v, 256).unwrap();
    assert_eq!(v, vec![0x80, 0x02]);
}

#[allow(dead_code)]
pub(crate) fn encode_sleb128(f: &mut impl Write, value: impl Into<i64>) -> std::io::Result<()> {
    let mut value = value.into();
    let mut more = true;
    while more {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if (value == 0 && byte & 0x40 == 0) || (value == -1 && byte & 0x40 != 0) {
            more = false;
        } else {
            byte |= 0x80;
        }
        f.write_all(&[byte])?;
    }
    return Ok(());
}

#[test]
fn test_sleb128() {
    let mut v = vec![];
    encode_sleb128(&mut v, -123456).unwrap();
    assert_eq!(v, vec![0xc0, 0xbb, 0x78]);
    assert_eq!(
        decode_sleb128(&mut std::io::Cursor::new(&v)).unwrap(),
        -123456
    );
}

pub(crate) fn decode_leb128(f: &mut impl Read) -> std::io::Result<u32> {
    let mut value = 0u32;
    let mut shift = 0;
    loop {
        let mut byte = [0u8];
        f.read_exact(&mut byte)?;
        value |= ((byte[0] & 0x7f) as u32) << shift;
        if byte[0] & 0x80 == 0 {
            return Ok(value);
        }
        shift += 7;
    }
}

#[allow(dead_code)]
pub(crate) fn decode_sleb128(f: &mut impl Read) -> std::io::Result<i32> {
    let mut value = 0u32;
    let mut shift = 0;
    let mut byte = [0u8];
    loop {
        f.read_exact(&mut byte)?;
        value |= ((byte[0] & 0x7f) as u32) << shift;
        shift += 7;
        if byte[0] & 0x80 == 0 {
            break;
        }
    }
    if shift < std::mem::size_of::<i32>() * 8 && byte[0] & 0x40 != 0 {
        value |= !0 << shift;
    }
    return Ok(value as i32);
}
