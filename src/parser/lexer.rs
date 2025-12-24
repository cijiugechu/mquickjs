use crate::cutils::from_hex;

fn is_num(c: u8) -> bool {
    c.is_ascii_digit()
}

// C: is_ident_first in mquickjs.c.
pub fn is_ident_first(c: u8) -> bool {
    c.is_ascii_alphabetic() || c == b'_' || c == b'$'
}

// C: is_ident_next in mquickjs.c.
pub fn is_ident_next(c: u8) -> bool {
    is_ident_first(c) || is_num(c)
}

// C: js_parse_escape in mquickjs.c. Returns escape value or -1/-2 on error.
pub fn js_parse_escape(buf: &[u8], plen: &mut usize) -> i32 {
    if buf.is_empty() {
        return -1;
    }
    let mut idx = 0usize;
    let c = buf[idx];
    idx += 1;
    let out = match c {
        b'b' => b'\x08' as i32,
        b'f' => b'\x0c' as i32,
        b'n' => b'\n' as i32,
        b'r' => b'\r' as i32,
        b't' => b'\t' as i32,
        b'v' => b'\x0b' as i32,
        b'\'' | b'\"' | b'\\' => c as i32,
        b'x' => {
            let h0 = match buf.get(idx) {
                Some(&b) => from_hex(b),
                None => return -1,
            };
            idx += 1;
            if h0 < 0 {
                return -1;
            }
            let h1 = match buf.get(idx) {
                Some(&b) => from_hex(b),
                None => return -1,
            };
            idx += 1;
            if h1 < 0 {
                return -1;
            }
            (h0 << 4) | h1
        }
        b'u' => {
            if buf.get(idx) == Some(&b'{') {
                idx += 1;
                let mut val: u32 = 0;
                loop {
                    let h = match buf.get(idx) {
                        Some(&b) => from_hex(b),
                        None => return -1,
                    };
                    if h < 0 {
                        return -1;
                    }
                    val = (val << 4) | (h as u32);
                    if val > 0x10ffff {
                        return -1;
                    }
                    idx += 1;
                    match buf.get(idx) {
                        Some(b'}') => {
                            idx += 1;
                            break;
                        }
                        Some(_) => {}
                        None => return -1,
                    }
                }
                val as i32
            } else {
                let mut val: u32 = 0;
                for _ in 0..4 {
                    let h = match buf.get(idx) {
                        Some(&b) => from_hex(b),
                        None => return -1,
                    };
                    if h < 0 {
                        return -1;
                    }
                    val = (val << 4) | (h as u32);
                    idx += 1;
                }
                val as i32
            }
        }
        b'0' => {
            if buf.get(idx).is_some_and(|b| is_num(*b)) {
                return -1;
            }
            0
        }
        _ => -2,
    };
    *plen = idx;
    out
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn ident_classification_matches_c() {
        assert!(is_ident_first(b'a'));
        assert!(is_ident_first(b'Z'));
        assert!(is_ident_first(b'_'));
        assert!(is_ident_first(b'$'));
        assert!(!is_ident_first(b'0'));
        assert!(is_ident_next(b'0'));
        assert!(!is_ident_next(b'-'));
    }

    #[test]
    fn parse_escape_simple_codes() {
        let mut len = 0;
        assert_eq!(js_parse_escape(b"n", &mut len), b'\n' as i32);
        assert_eq!(len, 1);
        assert_eq!(js_parse_escape(b"'", &mut len), b'\'' as i32);
        assert_eq!(len, 1);
        assert_eq!(js_parse_escape(b"\\", &mut len), b'\\' as i32);
        assert_eq!(len, 1);
    }

    #[test]
    fn parse_escape_hex_and_unicode() {
        let mut len = 0;
        assert_eq!(js_parse_escape(b"x41", &mut len), 0x41);
        assert_eq!(len, 3);
        assert_eq!(js_parse_escape(b"u0041", &mut len), 0x41);
        assert_eq!(len, 5);
        assert_eq!(js_parse_escape(b"u{1f600}", &mut len), 0x1f600);
        assert_eq!(len, 8);
    }

    #[test]
    fn parse_escape_zero_rules() {
        let mut len = 0;
        assert_eq!(js_parse_escape(b"0", &mut len), 0);
        assert_eq!(len, 1);
        len = 9;
        assert_eq!(js_parse_escape(b"09", &mut len), -1);
        assert_eq!(len, 9);
    }

    #[test]
    fn parse_escape_invalid_sequences() {
        let mut len = 7;
        assert_eq!(js_parse_escape(b"q", &mut len), -2);
        assert_eq!(len, 1);
        len = 7;
        assert_eq!(js_parse_escape(b"xZ1", &mut len), -1);
        assert_eq!(len, 7);
        len = 7;
        assert_eq!(js_parse_escape(b"u{110000}", &mut len), -1);
        assert_eq!(len, 7);
    }
}
