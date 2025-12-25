use crate::cutils::get_be32;
use crate::opcode::OPCODES;

fn get_bit(buf: &[u8], index: u32) -> u32 {
    let byte_index = (index >> 3) as usize;
    debug_assert!(byte_index < buf.len());
    let byte = buf.get(byte_index).copied().unwrap_or(0);
    ((byte >> (7 - (index & 7))) & 1) as u32
}

fn get_bits_slow(buf: &[u8], index: u32, n: u32) -> u32 {
    debug_assert!(n <= 32);
    let mut val = 0u32;
    for i in 0..n {
        val |= get_bit(buf, index + i) << (n - 1 - i);
    }
    val
}

fn get_bits(buf: &[u8], index: u32, n: u32) -> u32 {
    debug_assert!(n <= 32);
    let pos = (index >> 3) as usize;
    if n > 25 || pos.saturating_add(3) >= buf.len() {
        return get_bits_slow(buf, index, n);
    }
    let val = get_be32(&buf[pos..pos + 4]);
    let shift = 32 - (index & 7) - n;
    (val >> shift) & ((1u32 << n) - 1)
}

fn get_ugolomb(buf: &[u8], index: &mut u32) -> u32 {
    let bit_len = (buf.len() as u32).saturating_mul(8);
    let mut idx = *index;
    let mut i = 0u32;

    loop {
        if idx >= bit_len {
            *index = idx;
            return 0xffff_ffff;
        }
        let bit = get_bit(buf, idx);
        idx += 1;
        if bit != 0 {
            break;
        }
        i += 1;
        if i == 32 {
            *index = idx;
            return 0xffff_ffff;
        }
    }

    let v = if i == 0 {
        0
    } else {
        let val = get_bits(buf, idx, i);
        idx += i;
        ((1u32 << i) | val) - 1
    };
    *index = idx;
    v
}

fn get_sgolomb(buf: &[u8], index: &mut u32) -> i32 {
    let val = get_ugolomb(buf, index);
    (val >> 1) as i32 ^ -((val & 1) as i32)
}

// C: `get_pc2line_hoisted_code_len` in mquickjs.c.
pub fn get_pc2line_hoisted_code_len(buf: &[u8]) -> u32 {
    let mut i = buf.len();
    let mut v = 0u32;
    while i > 0 {
        i -= 1;
        v = (v << 7) | (buf[i] as u32 & 0x7f);
        if (buf[i] & 0x80) == 0 {
            break;
        }
    }
    v
}

// C: `get_pc2line` in mquickjs.c. Updates `line_num`, `col_num`, and bit `index`.
pub fn get_pc2line(
    line_num: &mut i32,
    col_num: &mut i32,
    buf: &[u8],
    index: &mut u32,
    has_column: bool,
) {
    let line_delta = get_sgolomb(buf, index);
    *line_num += line_delta;
    if has_column {
        if line_delta == 0 {
            let col_delta = get_sgolomb(buf, index);
            *col_num += col_delta;
        } else {
            let col = get_ugolomb(buf, index) as i32;
            *col_num = col + 1;
        }
    } else {
        *col_num = 0;
    }
}

// C: `find_line_col` in mquickjs.c. Returns `(0, 0)` if no position info is available.
pub fn find_line_col(
    byte_code: &[u8],
    pc2line: Option<&[u8]>,
    has_column: bool,
    pc: u32,
) -> (i32, i32) {
    let pc2line = match pc2line {
        Some(buf) => buf,
        None => return (0, 0),
    };
    if byte_code.is_empty() {
        return (0, 0);
    }

    let hoisted_len = get_pc2line_hoisted_code_len(pc2line);
    let mut pos = hoisted_len as usize;
    let pc = pc.max(hoisted_len);
    let mut pc2line_pos = 0u32;
    let mut line_num = 1i32;
    let mut col_num = 1i32;

    while pos < byte_code.len() {
        get_pc2line(&mut line_num, &mut col_num, pc2line, &mut pc2line_pos, has_column);
        if pos as u32 == pc {
            return (line_num, col_num);
        }
        let op = byte_code[pos] as usize;
        let size = match OPCODES.get(op) {
            Some(info) => info.size as usize,
            None => return (0, 0),
        };
        if size == 0 {
            return (0, 0);
        }
        pos += size;
    }
    (0, 0)
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use crate::opcode::OP_NOP;

    struct BitWriter {
        bytes: Vec<u8>,
        bit_len: u32,
    }

    impl BitWriter {
        fn new() -> Self {
            Self {
                bytes: Vec::new(),
                bit_len: 0,
            }
        }

        fn push_bits(&mut self, n: u32, bits: u32) {
            debug_assert!(n <= 32);
            for i in (0..n).rev() {
                let bit = (bits >> i) & 1;
                self.push_bit(bit as u8);
            }
        }

        fn push_bit(&mut self, bit: u8) {
            let byte_index = (self.bit_len >> 3) as usize;
            let bit_in_byte = 7 - (self.bit_len & 7);
            if byte_index == self.bytes.len() {
                self.bytes.push(0);
            }
            if bit != 0 {
                self.bytes[byte_index] |= 1 << bit_in_byte;
            }
            self.bit_len += 1;
        }

        fn pad_to_byte(&mut self) {
            let rem = self.bit_len & 7;
            if rem != 0 {
                self.push_bits(8 - rem, 0);
            }
        }

        fn into_bytes(self) -> Vec<u8> {
            self.bytes
        }
    }

    fn push_ugolomb(writer: &mut BitWriter, v: u32) {
        debug_assert!(v < u32::MAX);
        let v = v + 1;
        let n = 32 - v.leading_zeros();
        if n > 1 {
            writer.push_bits(n - 1, 0);
        }
        writer.push_bits(n, v);
    }

    fn push_sgolomb(writer: &mut BitWriter, v: i32) {
        let uv = ((v as u32) << 1) ^ ((v >> 31) as u32);
        push_ugolomb(writer, uv);
    }

    fn append_hoisted_len(buf: &mut Vec<u8>, mut n: u32) {
        let mut h = 0u32;
        loop {
            buf.push(((n & 0x7f) | h) as u8);
            n >>= 7;
            if n == 0 {
                break;
            }
            h |= 0x80;
        }
    }

    #[test]
    fn ugolomb_roundtrip() {
        let values = [0u32, 1, 2, 3, 7, 8, 63, 127, 128, 1024, 65535];
        let mut writer = BitWriter::new();
        for &v in &values {
            push_ugolomb(&mut writer, v);
        }
        let data = writer.into_bytes();
        let mut index = 0u32;
        for &v in &values {
            assert_eq!(get_ugolomb(&data, &mut index), v);
        }
    }

    #[test]
    fn sgolomb_roundtrip() {
        let values = [0i32, 1, -1, 2, -2, 15, -15, 1024, -1024];
        let mut writer = BitWriter::new();
        for &v in &values {
            push_sgolomb(&mut writer, v);
        }
        let data = writer.into_bytes();
        let mut index = 0u32;
        for &v in &values {
            assert_eq!(get_sgolomb(&data, &mut index), v);
        }
    }

    #[test]
    fn hoisted_code_len_roundtrip() {
        let values = [0u32, 1, 127, 128, 0x3fff, 0x1fffff];
        for &v in &values {
            let mut buf = Vec::new();
            append_hoisted_len(&mut buf, v);
            assert_eq!(get_pc2line_hoisted_code_len(&buf), v);
        }
    }

    #[test]
    fn find_line_col_basic() {
        let byte_code = vec![OP_NOP.0 as u8, OP_NOP.0 as u8, OP_NOP.0 as u8];
        let mut writer = BitWriter::new();

        push_sgolomb(&mut writer, 0);
        push_sgolomb(&mut writer, 0);
        push_sgolomb(&mut writer, 0);
        push_sgolomb(&mut writer, 2);
        push_sgolomb(&mut writer, 1);
        push_ugolomb(&mut writer, 0);

        writer.pad_to_byte();
        let mut pc2line = writer.into_bytes();
        append_hoisted_len(&mut pc2line, 0);

        assert_eq!(find_line_col(&byte_code, Some(&pc2line), true, 0), (1, 1));
        assert_eq!(find_line_col(&byte_code, Some(&pc2line), true, 1), (1, 3));
        assert_eq!(find_line_col(&byte_code, Some(&pc2line), true, 2), (2, 1));
    }

    #[test]
    fn find_line_col_missing_pc2line() {
        let byte_code = vec![OP_NOP.0 as u8];
        assert_eq!(find_line_col(&byte_code, None, true, 0), (0, 0));
    }
}
