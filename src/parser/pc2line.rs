use crate::cutils::{get_be32, put_be32};
use crate::opcode::OPCODES;
use super::pos::get_line_col_delta;
use super::types::SourcePos;

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

// C: pc2line_put_bits*, put_{u,sg}olomb, emit_pc2line in mquickjs.c.
pub struct Pc2LineEmitter {
    buf: Vec<u8>,
    bit_len: u32,
    source_pos: SourcePos,
    has_column: bool,
}

impl Pc2LineEmitter {
    pub fn new(source_pos: SourcePos, has_column: bool) -> Self {
        Self {
            buf: Vec::new(),
            bit_len: 0,
            source_pos,
            has_column,
        }
    }

    pub fn bit_len(&self) -> u32 {
        self.bit_len
    }

    pub fn source_pos(&self) -> SourcePos {
        self.source_pos
    }

    pub fn bytes(&self) -> &[u8] {
        &self.buf
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.buf
    }

    pub fn pad_to_byte(&mut self) {
        let rem = self.bit_len & 7;
        if rem != 0 {
            self.put_bits(8 - rem, 0);
        }
    }

    pub fn emit_pc2line(&mut self, source_buf: &[u8], pos: SourcePos) {
        let (line_delta, col_delta) = get_line_col_delta(source_buf, self.source_pos, pos);
        self.put_sgolomb(line_delta);
        if self.has_column {
            if line_delta == 0 {
                self.put_sgolomb(col_delta);
            } else {
                debug_assert!(col_delta >= 0);
                self.put_ugolomb(col_delta as u32);
            }
        }
        self.source_pos = pos;
    }

    pub fn restore_state(&mut self, bit_len: u32, source_pos: SourcePos) {
        self.bit_len = bit_len;
        self.source_pos = source_pos;
    }

    pub fn append_hoisted_len(&mut self, mut len: u32) {
        self.pad_to_byte();
        let mut high = 0u32;
        loop {
            let byte = (len & 0x7f) | high;
            self.put_bits(8, byte);
            len >>= 7;
            if len == 0 {
                break;
            }
            high |= 0x80;
        }
    }

    fn put_bits_short(&mut self, n: u32, bits: u32) {
        debug_assert!((1..=25).contains(&n));
        debug_assert!(bits < (1u32 << n));
        let index = self.bit_len;
        let pos = (index >> 3) as usize;
        let need = pos + 4;
        if self.buf.len() < need {
            self.buf.resize(need, 0);
        }
        let shift = 32 - (index & 7) - n;
        let mut val = get_be32(&self.buf[pos..pos + 4]);
        let mask = ((1u32 << n) - 1) << shift;
        val &= !mask;
        val |= (bits << shift) & mask;
        put_be32(&mut self.buf[pos..pos + 4], val);
        self.bit_len = index + n;
    }

    fn put_bits(&mut self, n: u32, bits: u32) {
        debug_assert!((1..=32).contains(&n));
        let n_max = 25;
        if n > n_max {
            self.put_bits_short(n - n_max, bits >> n_max);
            let low = bits & ((1u32 << n_max) - 1);
            self.put_bits_short(n_max, low);
        } else {
            self.put_bits_short(n, bits);
        }
    }

    fn put_ugolomb(&mut self, v: u32) {
        debug_assert!(v < u32::MAX);
        let v = v + 1;
        let n = 32 - v.leading_zeros();
        if n > 1 {
            self.put_bits(n - 1, 0);
        }
        self.put_bits(n, v);
    }

    fn put_sgolomb(&mut self, v: i32) {
        debug_assert!(v != i32::MIN);
        let v = v as u32;
        let sign = 0u32.wrapping_sub(v >> 31);
        let uv = v.wrapping_mul(2) ^ sign;
        self.put_ugolomb(uv);
    }
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
    use crate::parser::pos::get_line_col;

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
        let mut writer = Pc2LineEmitter::new(0, false);
        for &v in &values {
            writer.put_ugolomb(v);
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
        let mut writer = Pc2LineEmitter::new(0, false);
        for &v in &values {
            writer.put_sgolomb(v);
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
        let source_buf = b"abc\nxyz";
        let positions = [0u32, 2, 4];
        let mut emitter = Pc2LineEmitter::new(0, true);
        for &pos in &positions {
            emitter.emit_pc2line(source_buf, pos);
        }
        emitter.pad_to_byte();
        let mut pc2line = emitter.into_bytes();
        append_hoisted_len(&mut pc2line, 0);

        for (pc, &pos) in positions.iter().enumerate() {
            let (line, col) = get_line_col(&source_buf[..pos as usize]);
            let expected = (line + 1, col + 1);
            assert_eq!(
                find_line_col(&byte_code, Some(&pc2line), true, pc as u32),
                expected
            );
        }
    }

    #[test]
    fn find_line_col_missing_pc2line() {
        let byte_code = vec![OP_NOP.0 as u8];
        assert_eq!(find_line_col(&byte_code, None, true, 0), (0, 0));
    }
}
