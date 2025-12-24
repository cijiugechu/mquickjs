use crate::parser_types::SourcePos;

fn get_line_col_inner(buf: &[u8]) -> (i32, i32) {
    let mut line_num = 0i32;
    let mut col_num = 0i32;
    for &c in buf {
        if c == b'\n' {
            line_num += 1;
            col_num = 0;
        } else if !(0x80..0xc0).contains(&c) {
            col_num += 1;
        }
    }
    (line_num, col_num)
}

// C: `get_line_col` in mquickjs.c. Returns zero-based line/column.
pub fn get_line_col(buf: &[u8]) -> (i32, i32) {
    get_line_col_inner(buf)
}

// C: `get_line_col_delta` in mquickjs.c.
pub fn get_line_col_delta(buf: &[u8], pos1: SourcePos, pos2: SourcePos) -> (i32, i32) {
    let pos1 = pos1 as usize;
    let pos2 = pos2 as usize;
    debug_assert!(pos1 <= buf.len());
    debug_assert!(pos2 <= buf.len());

    if pos2 >= pos1 {
        get_line_col_inner(&buf[pos1..pos2])
    } else {
        let (mut line_num, mut col_num) = get_line_col_inner(&buf[pos2..pos1]);
        line_num = -line_num;
        col_num = -col_num;
        if line_num != 0 {
            col_num = 0;
            let mut idx = pos2 as isize - 1;
            while idx >= 0 {
                let c = buf[idx as usize];
                if c == b'\n' {
                    break;
                }
                if !(0x80..0xc0).contains(&c) {
                    col_num += 1;
                }
                idx -= 1;
            }
        }
        (line_num, col_num)
    }
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn line_col_basic() {
        let buf = b"abc\nxyz";
        assert_eq!(get_line_col(&buf[..0]), (0, 0));
        assert_eq!(get_line_col(&buf[..3]), (0, 3));
        assert_eq!(get_line_col(&buf[..4]), (1, 0));
        assert_eq!(get_line_col(&buf[..5]), (1, 1));
    }

    #[test]
    fn line_col_utf8_counts_lead_bytes() {
        let buf = b"caf\xC3\xA9";
        assert_eq!(get_line_col(buf), (0, 4));
    }

    #[test]
    fn line_col_delta_forward_same_line() {
        let buf = b"abcdef";
        assert_eq!(get_line_col_delta(buf, 1, 4), (0, 3));
    }

    #[test]
    fn line_col_delta_forward_multi_line() {
        let buf = b"ab\ncde\nfg";
        assert_eq!(get_line_col_delta(buf, 1, 4), (1, 1));
    }

    #[test]
    fn line_col_delta_backward_same_line() {
        let buf = b"ab\ncde\nfg";
        assert_eq!(get_line_col_delta(buf, 4, 3), (0, -1));
    }

    #[test]
    fn line_col_delta_backward_multi_line() {
        let buf = b"ab\ncde\nfg";
        assert_eq!(get_line_col_delta(buf, 7, 4), (-1, 1));
    }
}
