// C: regexp flag constants and js_parse_regexp_flags from mquickjs.c.

pub const LRE_FLAG_GLOBAL: u32 = 1 << 0;
pub const LRE_FLAG_IGNORECASE: u32 = 1 << 1;
pub const LRE_FLAG_MULTILINE: u32 = 1 << 2;
pub const LRE_FLAG_DOTALL: u32 = 1 << 3;
pub const LRE_FLAG_UNICODE: u32 = 1 << 4;
pub const LRE_FLAG_STICKY: u32 = 1 << 5;

// C: js_parse_regexp_flags in mquickjs.c.
pub fn parse_regexp_flags(pre_flags: &mut u32, buf: &[u8]) -> usize {
    let mut idx = 0usize;
    let mut re_flags = 0u32;

    while idx < buf.len() {
        let c = buf[idx];
        if c == 0 {
            break;
        }
        let mask = match c {
            b'g' => LRE_FLAG_GLOBAL,
            b'i' => LRE_FLAG_IGNORECASE,
            b'm' => LRE_FLAG_MULTILINE,
            b's' => LRE_FLAG_DOTALL,
            b'u' => LRE_FLAG_UNICODE,
            b'y' => LRE_FLAG_STICKY,
            _ => break,
        };
        if (re_flags & mask) != 0 {
            break;
        }
        re_flags |= mask;
        idx += 1;
    }

    *pre_flags = re_flags;
    idx
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn parse_regexp_flags_basic() {
        let mut flags = 0u32;
        let len = parse_regexp_flags(&mut flags, b"gim");
        assert_eq!(len, 3);
        assert_eq!(
            flags,
            LRE_FLAG_GLOBAL | LRE_FLAG_IGNORECASE | LRE_FLAG_MULTILINE
        );
    }

    #[test]
    fn parse_regexp_flags_stops_on_duplicates() {
        let mut flags = 0u32;
        let len = parse_regexp_flags(&mut flags, b"gg");
        assert_eq!(len, 1);
        assert_eq!(flags, LRE_FLAG_GLOBAL);
    }

    #[test]
    fn parse_regexp_flags_stops_on_unknown() {
        let mut flags = 0u32;
        let len = parse_regexp_flags(&mut flags, b"ga");
        assert_eq!(len, 1);
        assert_eq!(flags, LRE_FLAG_GLOBAL);
    }

    #[test]
    fn parse_regexp_flags_empty_input() {
        let mut flags = 0u32;
        let len = parse_regexp_flags(&mut flags, b"");
        assert_eq!(len, 0);
        assert_eq!(flags, 0);
    }
}
