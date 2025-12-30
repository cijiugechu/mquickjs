//! Utilities ported from `mquickjs-c/cutils.h` and `mquickjs-c/cutils.c`.

use zerocopy::byteorder::{big_endian, native_endian};

pub const UTF8_CHAR_LEN_MAX: usize = 4;

pub fn pstrcpy(buf: &mut [u8], src: &[u8]) {
    if buf.is_empty() {
        return;
    }

    let mut i = 0;
    while i + 1 < buf.len() && i < src.len() {
        let c = src[i];
        if c == 0 {
            break;
        }
        buf[i] = c;
        i += 1;
    }
    buf[i] = 0;
}

pub fn pstrcat<'a>(buf: &'a mut [u8], src: &[u8]) -> &'a mut [u8] {
    let mut len = 0;
    while len < buf.len() && buf[len] != 0 {
        len += 1;
    }
    if len < buf.len() {
        pstrcpy(&mut buf[len..], src);
    }
    buf
}

pub fn clz32(a: u32) -> u32 {
    debug_assert!(a != 0);
    a.leading_zeros()
}

pub fn clz64(a: u64) -> u32 {
    debug_assert!(a != 0);
    a.leading_zeros()
}

pub fn ctz32(a: u32) -> u32 {
    debug_assert!(a != 0);
    a.trailing_zeros()
}

pub fn ctz64(a: u64) -> u32 {
    debug_assert!(a != 0);
    a.trailing_zeros()
}

pub fn get_u64(tab: &[u8]) -> u64 {
    debug_assert!(tab.len() >= 8);
    let bytes = [tab[0], tab[1], tab[2], tab[3], tab[4], tab[5], tab[6], tab[7]];
    native_endian::U64::from_bytes(bytes).get()
}

pub fn get_i64(tab: &[u8]) -> i64 {
    debug_assert!(tab.len() >= 8);
    let bytes = [tab[0], tab[1], tab[2], tab[3], tab[4], tab[5], tab[6], tab[7]];
    native_endian::I64::from_bytes(bytes).get()
}

pub fn put_u64(tab: &mut [u8], val: u64) {
    debug_assert!(tab.len() >= 8);
    let bytes = native_endian::U64::new(val).to_bytes();
    tab[..8].copy_from_slice(&bytes);
}

pub fn get_u32(tab: &[u8]) -> u32 {
    debug_assert!(tab.len() >= 4);
    let bytes = [tab[0], tab[1], tab[2], tab[3]];
    native_endian::U32::from_bytes(bytes).get()
}

pub fn get_i32(tab: &[u8]) -> i32 {
    debug_assert!(tab.len() >= 4);
    let bytes = [tab[0], tab[1], tab[2], tab[3]];
    native_endian::I32::from_bytes(bytes).get()
}

pub fn put_u32(tab: &mut [u8], val: u32) {
    debug_assert!(tab.len() >= 4);
    let bytes = native_endian::U32::new(val).to_bytes();
    tab[..4].copy_from_slice(&bytes);
}

pub fn get_u16(tab: &[u8]) -> u32 {
    debug_assert!(tab.len() >= 2);
    let bytes = [tab[0], tab[1]];
    native_endian::U16::from_bytes(bytes).get() as u32
}

pub fn get_i16(tab: &[u8]) -> i32 {
    debug_assert!(tab.len() >= 2);
    let bytes = [tab[0], tab[1]];
    native_endian::I16::from_bytes(bytes).get() as i32
}

pub fn put_u16(tab: &mut [u8], val: u16) {
    debug_assert!(tab.len() >= 2);
    let bytes = native_endian::U16::new(val).to_bytes();
    tab[..2].copy_from_slice(&bytes);
}

pub fn get_u8(tab: &[u8]) -> u32 {
    debug_assert!(!tab.is_empty());
    tab[0] as u32
}

pub fn get_i8(tab: &[u8]) -> i32 {
    debug_assert!(!tab.is_empty());
    (tab[0] as i8) as i32
}

pub fn put_u8(tab: &mut [u8], val: u8) {
    debug_assert!(!tab.is_empty());
    tab[0] = val;
}

pub fn bswap16(x: u16) -> u16 {
    x.swap_bytes()
}

pub fn bswap32(x: u32) -> u32 {
    x.swap_bytes()
}

pub fn bswap64(x: u64) -> u64 {
    x.swap_bytes()
}

pub fn get_be32(tab: &[u8]) -> u32 {
    debug_assert!(tab.len() >= 4);
    let bytes = [tab[0], tab[1], tab[2], tab[3]];
    big_endian::U32::from_bytes(bytes).get()
}

pub fn put_be32(tab: &mut [u8], val: u32) {
    debug_assert!(tab.len() >= 4);
    let bytes = big_endian::U32::new(val).to_bytes();
    tab[..4].copy_from_slice(&bytes);
}

pub fn unicode_to_utf8(buf: &mut [u8], c: u32) -> usize {
    if c < 0x80 {
        debug_assert!(!buf.is_empty());
        buf[0] = c as u8;
        1
    } else {
        unicode_to_utf8_slow(buf, c)
    }
}

fn unicode_to_utf8_slow(buf: &mut [u8], c: u32) -> usize {
    debug_assert!(buf.len() >= UTF8_CHAR_LEN_MAX);

    if c < 0x800 {
        buf[0] = ((c >> 6) | 0xc0) as u8;
        buf[1] = ((c & 0x3f) | 0x80) as u8;
        2
    } else if c < 0x10000 {
        buf[0] = ((c >> 12) | 0xe0) as u8;
        buf[1] = (((c >> 6) & 0x3f) | 0x80) as u8;
        buf[2] = ((c & 0x3f) | 0x80) as u8;
        3
    } else if c < 0x0020_0000 {
        buf[0] = ((c >> 18) | 0xf0) as u8;
        buf[1] = (((c >> 12) & 0x3f) | 0x80) as u8;
        buf[2] = (((c >> 6) & 0x3f) | 0x80) as u8;
        buf[3] = ((c & 0x3f) | 0x80) as u8;
        4
    } else {
        0
    }
}

pub fn unicode_from_utf8(buf: &[u8], max_len: usize, plen: &mut usize) -> i32 {
    if buf.is_empty() {
        *plen = 0;
        return -1;
    }

    if buf[0] < 0x80 {
        *plen = 1;
        return buf[0] as i32;
    }

    unicode_from_utf8_slow(buf, max_len, plen)
}

fn unicode_from_utf8_slow(buf: &[u8], max_len: usize, plen: &mut usize) -> i32 {
    let mut len = 1;
    let mut c = buf[0] as u32;

    if c < 0xc0 {
        *plen = len;
        return -1;
    } else if c < 0xe0 {
        if max_len < 2 || (buf.get(1).copied().unwrap_or(0) & 0xc0) != 0x80 {
            *plen = len;
            return -1;
        }
        c = ((buf[0] as u32 & 0x1f) << 6) | (buf[1] as u32 & 0x3f);
        len = 2;
        if c < 0x80 {
            *plen = len;
            return -1;
        }
    } else if c < 0xf0 {
        if max_len < 2 || (buf.get(1).copied().unwrap_or(0) & 0xc0) != 0x80 {
            *plen = len;
            return -1;
        }
        if max_len < 3 || (buf.get(2).copied().unwrap_or(0) & 0xc0) != 0x80 {
            len = 2;
            *plen = len;
            return -1;
        }
        c = ((buf[0] as u32 & 0x0f) << 12)
            | ((buf[1] as u32 & 0x3f) << 6)
            | (buf[2] as u32 & 0x3f);
        len = 3;
        if c < 0x800 {
            *plen = len;
            return -1;
        }
    } else if c < 0xf8 {
        if max_len < 2 || (buf.get(1).copied().unwrap_or(0) & 0xc0) != 0x80 {
            *plen = len;
            return -1;
        }
        if max_len < 3 || (buf.get(2).copied().unwrap_or(0) & 0xc0) != 0x80 {
            len = 2;
            *plen = len;
            return -1;
        }
        if max_len < 4 || (buf.get(3).copied().unwrap_or(0) & 0xc0) != 0x80 {
            len = 3;
            *plen = len;
            return -1;
        }
        c = ((buf[0] as u32 & 0x07) << 18)
            | ((buf[1] as u32 & 0x3f) << 12)
            | ((buf[2] as u32 & 0x3f) << 6)
            | (buf[3] as u32 & 0x3f);
        len = 4;
        if !(0x10000..=0x10ffff).contains(&c) {
            *plen = len;
            return -1;
        }
    } else {
        *plen = len;
        return -1;
    }

    *plen = len;
    c as i32
}

pub fn utf8_get(buf: &[u8], plen: &mut usize) -> i32 {
    if buf.is_empty() {
        *plen = 0;
        return -1;
    }
    if buf[0] < 0x80 {
        *plen = 1;
        return buf[0] as i32;
    }
    utf8_get_slow(buf, plen)
}

fn utf8_get_slow(buf: &[u8], plen: &mut usize) -> i32 {
    let c0 = buf[0] as u32;
    if c0 < 0xc0 {
        *plen = 1;
        return c0 as i32;
    } else if c0 < 0xe0 {
        debug_assert!(buf.len() >= 2);
        let c = ((buf[0] as u32 & 0x1f) << 6) | (buf[1] as u32 & 0x3f);
        *plen = 2;
        return c as i32;
    } else if c0 < 0xf0 {
        debug_assert!(buf.len() >= 3);
        let c = ((buf[0] as u32 & 0x0f) << 12)
            | ((buf[1] as u32 & 0x3f) << 6)
            | (buf[2] as u32 & 0x3f);
        *plen = 3;
        return c as i32;
    } else if c0 < 0xf8 {
        debug_assert!(buf.len() >= 4);
        let c = ((buf[0] as u32 & 0x07) << 18)
            | ((buf[1] as u32 & 0x3f) << 12)
            | ((buf[2] as u32 & 0x3f) << 6)
            | (buf[3] as u32 & 0x3f);
        *plen = 4;
        return c as i32;
    }

    *plen = 1;
    c0 as i32
}

pub fn from_hex(c: u8) -> i32 {
    match c {
        b'0'..=b'9' => (c - b'0') as i32,
        b'A'..=b'F' => (c - b'A' + 10) as i32,
        b'a'..=b'f' => (c - b'a' + 10) as i32,
        _ => -1,
    }
}

pub fn float64_as_uint64(d: f64) -> u64 {
    d.to_bits()
}

pub fn uint64_as_float64(u64: u64) -> f64 {
    f64::from_bits(u64)
}

pub fn float_as_uint(f: f32) -> u32 {
    f.to_bits()
}

pub fn uint_as_float(v: u32) -> f32 {
    f32::from_bits(v)
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn pstrcpy_truncates_and_nuls() {
        let mut buf = [0u8; 4];
        pstrcpy(&mut buf, b"hello");
        assert_eq!(&buf, b"hel\0");
    }

    #[test]
    fn pstrcat_appends_within_bounds() {
        let mut buf = [0u8; 6];
        pstrcpy(&mut buf, b"hi");
        pstrcat(&mut buf, b"42");
        assert_eq!(&buf, b"hi42\0\0");
    }

    #[test]
    fn bit_ops_match_rust() {
        assert_eq!(clz32(1), 31);
        assert_eq!(ctz32(8), 3);
        assert_eq!(clz64(1), 63);
        assert_eq!(ctz64(16), 4);
    }

    #[test]
    fn read_write_native_endian() {
        let mut buf = [0u8; 8];
        put_u16(&mut buf, 0x1122);
        assert_eq!(get_u16(&buf), 0x1122);
        put_u32(&mut buf, 0x11223344);
        assert_eq!(get_u32(&buf), 0x11223344);
        put_u64(&mut buf, 0x1122334455667788);
        assert_eq!(get_u64(&buf), 0x1122334455667788);
    }

    #[test]
    fn read_write_big_endian() {
        let mut buf = [0u8; 4];
        put_be32(&mut buf, 0x01020304);
        assert_eq!(buf, [1, 2, 3, 4]);
        assert_eq!(get_be32(&buf), 0x01020304);
    }

    #[test]
    fn unicode_to_utf8_ascii_and_four_bytes() {
        let mut buf = [0u8; UTF8_CHAR_LEN_MAX];
        let len = unicode_to_utf8(&mut buf, 0x7f);
        assert_eq!(len, 1);
        assert_eq!(buf[0], 0x7f);

        let len = unicode_to_utf8(&mut buf, 0x1f642); // U+1F642
        assert_eq!(len, 4);
        assert_eq!(&buf[..4], [0xf0, 0x9f, 0x99, 0x82]);
    }

    #[test]
    fn unicode_to_utf8_rejects_out_of_range() {
        let mut buf = [0u8; UTF8_CHAR_LEN_MAX];
        assert_eq!(unicode_to_utf8(&mut buf, 0x0020_0000), 0);
    }

    #[test]
    fn unicode_from_utf8_reports_len_on_error() {
        let mut len = 0;
        let c = unicode_from_utf8(&[0xc0, 0x80], 2, &mut len);
        assert_eq!(c, -1);
        assert_eq!(len, 2);

        let c = unicode_from_utf8(&[0xe0], 1, &mut len);
        assert_eq!(c, -1);
        assert_eq!(len, 1);
    }

    #[test]
    fn unicode_from_utf8_accepts_surrogates() {
        let mut len = 0;
        let c = unicode_from_utf8(&[0xed, 0xa0, 0x80], 3, &mut len); // U+D800
        assert_eq!(c, 0xd800);
        assert_eq!(len, 3);
    }

    #[test]
    fn utf8_get_matches_c_behavior() {
        let mut len = 0;
        let c = utf8_get(&[0x41], &mut len);
        assert_eq!(c, 0x41);
        assert_eq!(len, 1);

        let c = utf8_get(&[0xc2, 0xa9, 0, 0], &mut len);
        assert_eq!(c, 0xa9);
        assert_eq!(len, 2);

        let c = utf8_get(&[0xf8, 0, 0, 0], &mut len);
        assert_eq!(c, 0xf8);
        assert_eq!(len, 1);
    }

    #[test]
    fn hex_helpers() {
        assert_eq!(from_hex(b'0'), 0);
        assert_eq!(from_hex(b'A'), 10);
        assert_eq!(from_hex(b'f'), 15);
        assert_eq!(from_hex(b'?'), -1);
    }

}
