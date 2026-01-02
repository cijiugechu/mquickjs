use mquickjs::cutils::{unicode_to_utf8, utf8_get, UTF8_CHAR_LEN_MAX};
use std::cmp::min;
use std::io::{self, Write};
use std::ptr::NonNull;

pub type ReadlineGetColor = fn(plen: &mut i32, buf: &[u8], pos: usize, buf_len: usize) -> i32;

pub const COLOR_NONE: i32 = 0;
pub const COLOR_GREEN: i32 = 3;
pub const COLOR_WHITE: i32 = 8;
pub const COLOR_BRIGHT_RED: i32 = 10;
pub const COLOR_BRIGHT_GREEN: i32 = 11;
pub const COLOR_BRIGHT_YELLOW: i32 = 12;
pub const COLOR_BRIGHT_MAGENTA: i32 = 14;
pub const COLOR_BRIGHT_CYAN: i32 = 15;
pub const COLOR_BRIGHT_WHITE: i32 = 16;

pub const TERM_COLORS: [&str; 17] = [
    "\x1b[0m",
    "\x1b[30m",
    "\x1b[31m",
    "\x1b[32m",
    "\x1b[33m",
    "\x1b[34m",
    "\x1b[35m",
    "\x1b[36m",
    "\x1b[37m",
    "\x1b[30;1m",
    "\x1b[31;1m",
    "\x1b[32;1m",
    "\x1b[33;1m",
    "\x1b[34;1m",
    "\x1b[35;1m",
    "\x1b[36;1m",
    "\x1b[37;1m",
];

pub const READLINE_RET_EXIT: i32 = -1;
pub const READLINE_RET_NOT_HANDLED: i32 = 0;
pub const READLINE_RET_HANDLED: i32 = 1;
pub const READLINE_RET_ACCEPTED: i32 = 2;

const IS_NORM: u8 = 0;
const IS_ESC: u8 = 1;
const IS_CSI: u8 = 2;

pub struct ReadlineState {
    pub term_cmd_buf_index: usize,
    pub term_cmd_buf_len: usize,
    pub utf8_val: u32,
    pub term_cmd_updated: bool,
    pub utf8_state: u8,
    pub term_esc_state: u8,
    pub term_esc_param: i32,
    pub term_esc_param1: i32,
    pub term_esc_param2: i32,
    pub term_cursor_x: i32,
    pub term_cursor_pos: i32,
    pub term_hist_entry: Option<usize>,
    pub term_history_size: usize,
    pub term_is_password: bool,
    pub term_prompt: Option<String>,
    pub term_width: i32,
    pub term_cmd_buf_size: usize,
    pub term_kill_buf_len: usize,
    pub term_cmd_buf: Option<NonNull<u8>>,
    pub term_kill_buf: Option<NonNull<u8>>,
    pub term_history_buf_size: usize,
    pub term_history: Option<NonNull<u8>>,
    pub get_color: Option<ReadlineGetColor>,
}

impl Default for ReadlineState {
    fn default() -> Self {
        Self {
            term_cmd_buf_index: 0,
            term_cmd_buf_len: 0,
            utf8_val: 0,
            term_cmd_updated: false,
            utf8_state: 0,
            term_esc_state: IS_NORM,
            term_esc_param: 0,
            term_esc_param1: 0,
            term_esc_param2: 0,
            term_cursor_x: 0,
            term_cursor_pos: 0,
            term_hist_entry: None,
            term_history_size: 0,
            term_is_password: false,
            term_prompt: None,
            term_width: 0,
            term_cmd_buf_size: 0,
            term_kill_buf_len: 0,
            term_cmd_buf: None,
            term_kill_buf: None,
            term_history_buf_size: 0,
            term_history: None,
            get_color: None,
        }
    }
}

impl ReadlineState {
    pub fn cmd_buf_slice(&self) -> &[u8] {
        let ptr = self.term_cmd_buf.expect("cmd buffer not initialized");
        unsafe { std::slice::from_raw_parts(ptr.as_ptr(), self.term_cmd_buf_len) }
    }

    fn cmd_buf_ro(&self) -> &[u8] {
        let ptr = self.term_cmd_buf.expect("cmd buffer not initialized");
        unsafe { std::slice::from_raw_parts(ptr.as_ptr(), self.term_cmd_buf_size) }
    }

    fn cmd_buf(&mut self) -> &mut [u8] {
        let ptr = self.term_cmd_buf.expect("cmd buffer not initialized");
        unsafe { std::slice::from_raw_parts_mut(ptr.as_ptr(), self.term_cmd_buf_size) }
    }

    fn kill_buf_ro(&self) -> &[u8] {
        let ptr = self.term_kill_buf.expect("kill buffer not initialized");
        unsafe { std::slice::from_raw_parts(ptr.as_ptr(), self.term_cmd_buf_size) }
    }

    fn kill_buf(&mut self) -> &mut [u8] {
        let ptr = self.term_kill_buf.expect("kill buffer not initialized");
        unsafe { std::slice::from_raw_parts_mut(ptr.as_ptr(), self.term_cmd_buf_size) }
    }

    fn history_buf_ro(&self) -> &[u8] {
        let ptr = self.term_history.expect("history buffer not initialized");
        unsafe { std::slice::from_raw_parts(ptr.as_ptr(), self.term_history_buf_size) }
    }

    fn history_buf(&mut self) -> &mut [u8] {
        let ptr = self.term_history.expect("history buffer not initialized");
        unsafe { std::slice::from_raw_parts_mut(ptr.as_ptr(), self.term_history_buf_size) }
    }
}

macro_rules! term_printf {
    ($($arg:tt)*) => {{
        let _ = io::stdout().write_fmt(format_args!($($arg)*));
    }};
}

fn term_flush() {
    let _ = io::stdout().flush();
}

fn term_write_bytes(bytes: &[u8]) {
    let _ = io::stdout().write_all(bytes);
}

fn term_show_prompt2(s: &mut ReadlineState) {
    let prompt = s.term_prompt.as_deref().unwrap_or("");
    term_printf!("{prompt}");
    term_flush();
    let width = s.term_width.max(1);
    s.term_cursor_x = (prompt.len() as i32) % width;
    s.term_cursor_pos = 0;
    s.term_esc_state = IS_NORM;
    s.utf8_state = 0;
}

fn term_show_prompt(s: &mut ReadlineState) {
    term_show_prompt2(s);
    s.term_cmd_buf_index = 0;
    s.term_cmd_buf_len = 0;
}

fn print_csi(n: i32, code: char) {
    if n == 1 {
        term_printf!("\x1b[{code}");
    } else {
        term_printf!("\x1b[{n}{code}");
    }
}

fn print_color(color: i32) {
    let idx = color as usize;
    if idx < TERM_COLORS.len() {
        term_printf!("{}", TERM_COLORS[idx]);
    }
}

fn move_cursor(s: &mut ReadlineState, mut delta: i32) {
    let width = s.term_width.max(1);
    if delta > 0 {
        while delta != 0 {
            if s.term_cursor_x == width - 1 {
                term_printf!("\r\n");
                s.term_cursor_x = 0;
                delta -= 1;
            } else {
                let l = min(width - 1 - s.term_cursor_x, delta);
                print_csi(l, 'C');
                delta -= l;
                s.term_cursor_x += l;
            }
        }
    } else if delta < 0 {
        delta = -delta;
        while delta != 0 {
            if s.term_cursor_x == 0 {
                print_csi(1, 'A');
                print_csi(width - 1, 'C');
                delta -= 1;
                s.term_cursor_x = width - 1;
            } else {
                let l = min(delta, s.term_cursor_x);
                print_csi(l, 'D');
                delta -= l;
                s.term_cursor_x -= l;
            }
        }
    }
}

fn char_width(c: i32) -> i32 {
    if c < 0x100 {
        1
    } else if (0x4E00..=0x9FFF).contains(&c)
        || (0xFF01..=0xFF5E).contains(&c)
        || (0x1F600..=0x1F64F).contains(&c)
    {
        2
    } else {
        1
    }
}

fn term_update(s: &mut ReadlineState) {
    let mut new_cursor_pos = 0;
    if s.term_cmd_updated {
        move_cursor(s, -s.term_cursor_pos);
        s.term_cursor_pos = 0;
        let mut last_color = COLOR_NONE;
        let mut color_len = 0;
        if s.term_cmd_buf_len < s.term_cmd_buf_size {
            let len = s.term_cmd_buf_len;
            let buf = s.cmd_buf();
            buf[len] = 0;
        }
        let mut i = 0;
        while i < s.term_cmd_buf_len {
            if i == s.term_cmd_buf_index {
                new_cursor_pos = s.term_cursor_pos;
            }
            let mut c_len = 0;
            let c = {
                let buf = s.cmd_buf_ro();
                utf8_get(&buf[i..], &mut c_len)
            };
            let mut out = [0u8; UTF8_CHAR_LEN_MAX + 1];
            let len = if s.term_is_password {
                out[0] = b'*';
                out[1] = 0;
                1
            } else {
                let buf = s.cmd_buf_ro();
                out[..c_len].copy_from_slice(&buf[i..i + c_len]);
                out[c_len] = 0;
                char_width(c)
            };
            if s.term_cursor_x + len > s.term_width && i > 0 {
                while s.term_cursor_x < s.term_width {
                    term_printf!(" ");
                    s.term_cursor_x += 1;
                    s.term_cursor_pos += 1;
                }
                s.term_cursor_x = 0;
            }
            s.term_cursor_pos += len;
            s.term_cursor_x += len;
            if s.term_cursor_x >= s.term_width {
                s.term_cursor_x = 0;
            }
            if !s.term_is_password && let Some(get_color) = s.get_color {
                if color_len == 0 {
                    let buf = s.cmd_buf_ro();
                    let color = get_color(&mut color_len, buf, i, s.term_cmd_buf_len);
                    if color != last_color {
                        last_color = color;
                        print_color(COLOR_NONE);
                        print_color(last_color);
                    }
                }
                color_len -= 1;
            }
            term_write_bytes(&out[..c_len]);
            i += c_len;
        }
        if last_color != COLOR_NONE {
            print_color(COLOR_NONE);
        }
        if i == s.term_cmd_buf_index {
            new_cursor_pos = s.term_cursor_pos;
        }
        if s.term_cursor_x == 0 {
            term_printf!(" \x08");
        }
        print_csi(1, 'J');
        s.term_cmd_updated = false;
    } else {
        let width = s.term_width.max(1);
        let mut cursor_x = (s.term_cursor_x - s.term_cursor_pos) % width;
        if cursor_x < 0 {
            cursor_x += width;
        }
        new_cursor_pos = 0;
        let mut i = 0;
        while i < s.term_cmd_buf_index {
            let mut c_len = 0;
            let c = {
                let buf = s.cmd_buf_ro();
                utf8_get(&buf[i..], &mut c_len)
            };
            let len = if s.term_is_password {
                char_width(b'*' as i32)
            } else {
                char_width(c)
            };
            if cursor_x + len > width && i > 0 {
                new_cursor_pos += width - cursor_x;
                cursor_x = 0;
            }
            new_cursor_pos += len;
            cursor_x += len;
            if cursor_x >= width {
                cursor_x = 0;
            }
            i += c_len;
        }
    }
    move_cursor(s, new_cursor_pos - s.term_cursor_pos);
    s.term_cursor_pos = new_cursor_pos;
    term_flush();
}

fn term_kill_region(s: &mut ReadlineState, to: usize, kill: bool) {
    let mut start = s.term_cmd_buf_index;
    let mut end = s.term_cmd_buf_index;
    if to < start {
        start = to;
    } else {
        end = to;
    }
    if end > s.term_cmd_buf_len {
        end = s.term_cmd_buf_len;
    }
    if start < end {
        let len = end - start;
        if kill {
            let data = {
                let buf = s.cmd_buf_ro();
                buf[start..start + len].to_vec()
            };
            {
                let kill_buf = s.kill_buf();
                kill_buf[..len].copy_from_slice(&data);
            }
            s.term_kill_buf_len = len;
        }
        let cmd_len = s.term_cmd_buf_len;
        let buf = s.cmd_buf();
        buf.copy_within(end..cmd_len, start);
        s.term_cmd_buf_len -= len;
        s.term_cmd_buf_index = start;
        s.term_cmd_updated = true;
    }
}

fn term_insert_region(s: &mut ReadlineState, p: &[u8]) {
    let pos = s.term_cmd_buf_index;
    let len = p.len();
    if pos + len < s.term_cmd_buf_size {
        let nchars = s.term_cmd_buf_len.saturating_sub(pos);
        if nchars > 0 {
            let buf = s.cmd_buf();
            buf.copy_within(pos..pos + nchars, pos + len);
        }
        let buf = s.cmd_buf();
        buf[pos..pos + len].copy_from_slice(p);
        s.term_cmd_buf_len += len;
        s.term_cmd_buf_index += len;
        s.term_cmd_updated = true;
    }
}

fn term_insert_char(s: &mut ReadlineState, ch: i32) {
    let mut buf = [0u8; UTF8_CHAR_LEN_MAX + 1];
    let len = unicode_to_utf8(&mut buf, ch as u32);
    if len > 0 {
        term_insert_region(s, &buf[..len]);
    }
}

fn is_utf8_ext(c: u8) -> bool {
    (0x80..0xC0).contains(&c)
}

fn term_backward_char(s: &mut ReadlineState) {
    if s.term_cmd_buf_index > 0 {
        s.term_cmd_buf_index -= 1;
        while s.term_cmd_buf_index > 0 {
            let is_ext = {
                let buf = s.cmd_buf_ro();
                is_utf8_ext(buf[s.term_cmd_buf_index])
            };
            if !is_ext {
                break;
            }
            s.term_cmd_buf_index -= 1;
        }
    }
}

fn term_forward_char(s: &mut ReadlineState) {
    if s.term_cmd_buf_index < s.term_cmd_buf_len {
        let c_len = {
            let mut c_len = 0;
            let buf = s.cmd_buf_ro();
            utf8_get(&buf[s.term_cmd_buf_index..], &mut c_len);
            c_len
        };
        s.term_cmd_buf_index += c_len;
    }
}

fn term_delete_char(s: &mut ReadlineState) {
    if s.term_cmd_buf_index < s.term_cmd_buf_len {
        let c_len = {
            let mut c_len = 0;
            let buf = s.cmd_buf_ro();
            utf8_get(&buf[s.term_cmd_buf_index..], &mut c_len);
            c_len
        };
        term_kill_region(s, s.term_cmd_buf_index + c_len, false);
    }
}

fn term_backspace(s: &mut ReadlineState) {
    if s.term_cmd_buf_index > 0 {
        term_backward_char(s);
        term_delete_char(s);
    }
}

fn is_space(c: u8) -> bool {
    c.is_ascii_whitespace()
}

fn skip_word_backward(s: &mut ReadlineState) -> usize {
    let mut pos = s.term_cmd_buf_index;
    while pos > 0 {
        let buf = s.cmd_buf_ro();
        if !is_space(buf[pos - 1]) {
            break;
        }
        pos -= 1;
    }
    while pos > 0 {
        let buf = s.cmd_buf_ro();
        if is_space(buf[pos - 1]) {
            break;
        }
        pos -= 1;
    }
    pos
}

fn skip_word_forward(s: &mut ReadlineState) -> usize {
    let mut pos = s.term_cmd_buf_index;
    while pos < s.term_cmd_buf_len {
        let buf = s.cmd_buf_ro();
        if !is_space(buf[pos]) {
            break;
        }
        pos += 1;
    }
    while pos < s.term_cmd_buf_len {
        let buf = s.cmd_buf_ro();
        if is_space(buf[pos]) {
            break;
        }
        pos += 1;
    }
    pos
}

fn term_skip_word_backward(s: &mut ReadlineState) {
    s.term_cmd_buf_index = skip_word_backward(s);
}

fn term_skip_word_forward(s: &mut ReadlineState) {
    s.term_cmd_buf_index = skip_word_forward(s);
}

fn term_yank(s: &mut ReadlineState) {
    let len = s.term_kill_buf_len;
    let data = {
        let buf = s.kill_buf_ro();
        buf[..len].to_vec()
    };
    term_insert_region(s, &data);
}

fn term_kill_word(s: &mut ReadlineState) {
    let to = skip_word_forward(s);
    term_kill_region(s, to, true);
}

fn term_kill_word_backward(s: &mut ReadlineState) {
    let to = skip_word_backward(s);
    term_kill_region(s, to, true);
}

fn term_bol(s: &mut ReadlineState) {
    s.term_cmd_buf_index = 0;
}

fn term_eol(s: &mut ReadlineState) {
    s.term_cmd_buf_index = s.term_cmd_buf_len;
}

fn history_entry_len(history: &[u8], start: usize, max: usize) -> usize {
    let mut idx = start;
    while idx < max && history[idx] != 0 {
        idx += 1;
    }
    idx - start
}

fn update_cmdline_from_history(s: &mut ReadlineState) {
    let Some(entry) = s.term_hist_entry else {
        return;
    };
    if entry >= s.term_history_size {
        return;
    }
    let history_size = s.term_history_size;
    let entry_bytes = {
        let history = s.history_buf_ro();
        let hist_entry_size = history_entry_len(history, entry, history_size);
        history[entry..entry + hist_entry_size].to_vec()
    };
    {
        let buf = s.cmd_buf();
        buf[..entry_bytes.len()].copy_from_slice(&entry_bytes);
    }
    s.term_cmd_buf_len = entry_bytes.len();
    s.term_cmd_buf_index = s.term_cmd_buf_len;
    s.term_cmd_updated = true;
}

fn term_up_char(s: &mut ReadlineState) {
    if s.term_hist_entry.is_none() {
        s.term_hist_entry = Some(s.term_history_size);
    }
    let Some(entry) = s.term_hist_entry else {
        return;
    };
    if entry == 0 {
        return;
    }
    let idx = {
        let history = s.history_buf_ro();
        let mut idx = entry - 1;
        while idx > 0 && history[idx - 1] != 0 {
            idx -= 1;
        }
        idx
    };
    s.term_hist_entry = Some(idx);
    update_cmdline_from_history(s);
}

fn term_down_char(s: &mut ReadlineState) {
    let Some(entry) = s.term_hist_entry else {
        return;
    };
    if entry >= s.term_history_size {
        s.term_hist_entry = None;
        s.term_cmd_buf_index = s.term_cmd_buf_len;
        return;
    }
    let history_size = s.term_history_size;
    let hist_entry_size = {
        let history = s.history_buf_ro();
        history_entry_len(history, entry, history_size) + 1
    };
    if entry + hist_entry_size < history_size {
        s.term_hist_entry = Some(entry + hist_entry_size);
        update_cmdline_from_history(s);
    } else {
        s.term_hist_entry = None;
        s.term_cmd_buf_index = s.term_cmd_buf_len;
    }
}

fn term_hist_add(s: &mut ReadlineState, cmdline: &[u8]) {
    if cmdline.is_empty() {
        return;
    }
    let cmdline_size = cmdline.len() + 1;
    let mut cmdline_with_nul = Vec::with_capacity(cmdline_size);
    cmdline_with_nul.extend_from_slice(cmdline);
    cmdline_with_nul.push(0);

    if let Some(entry) = s.term_hist_entry
        && entry < s.term_history_size
    {
        let history_size = s.term_history_size;
        let hist_entry_size = {
            let history = s.history_buf_ro();
            history_entry_len(history, entry, history_size) + 1
        };
        if hist_entry_size == cmdline_size
            && s.history_buf_ro()[entry..entry + hist_entry_size] == cmdline_with_nul[..]
        {
            {
                let history_size = s.term_history_size;
                let history = s.history_buf();
                history.copy_within(entry + hist_entry_size..history_size, entry);
            }
            s.term_history_size -= hist_entry_size;
        }
    }

    let mut idx = 0;
    while idx < s.term_history_size {
        let history_size = s.term_history_size;
        let hist_entry_size = {
            let history = s.history_buf_ro();
            history_entry_len(history, idx, history_size) + 1
        };
        let matched = hist_entry_size == cmdline_size
            && s.history_buf_ro()[idx..idx + hist_entry_size] == cmdline_with_nul[..];
        if matched {
            {
                let history_size = s.term_history_size;
                let history = s.history_buf();
                history.copy_within(idx + hist_entry_size..history_size, idx);
            }
            s.term_history_size -= hist_entry_size;
            break;
        }
        idx += hist_entry_size;
    }

    if cmdline_size <= s.term_history_buf_size {
        while s.term_history_size + cmdline_size > s.term_history_buf_size {
            let history_size = s.term_history_size;
            let hist_entry_size = {
                let history = s.history_buf_ro();
                history_entry_len(history, 0, history_size) + 1
            };
            {
                let history_size = s.term_history_size;
                let history = s.history_buf();
                history.copy_within(hist_entry_size..history_size, 0);
            }
            s.term_history_size -= hist_entry_size;
        }
        let history_size = s.term_history_size;
        let history = s.history_buf();
        history[history_size..history_size + cmdline_size]
            .copy_from_slice(&cmdline_with_nul);
        s.term_history_size += cmdline_size;
    }
    s.term_hist_entry = None;
}

fn term_return(s: &mut ReadlineState) {
    if s.term_cmd_buf_len < s.term_cmd_buf_size {
        {
            let len = s.term_cmd_buf_len;
            let buf = s.cmd_buf();
            buf[len] = 0;
        }
    }
    if !s.term_is_password {
        let cmdline = s.cmd_buf_slice().to_vec();
        term_hist_add(s, &cmdline);
    }
    s.term_cmd_buf_index = s.term_cmd_buf_len;
}

fn readline_handle_char(s: &mut ReadlineState, ch: i32) -> i32 {
    let mut ret = READLINE_RET_HANDLED;
    match s.term_esc_state {
        IS_NORM => match ch {
            1 => term_bol(s),
            4 => {
                if s.term_cmd_buf_len == 0 {
                    term_printf!("^D\n");
                    return READLINE_RET_EXIT;
                }
                term_delete_char(s);
            }
            5 => term_eol(s),
            9 => {}
            10 | 13 => {
                term_return(s);
                ret = READLINE_RET_ACCEPTED;
            }
            11 => term_kill_region(s, s.term_cmd_buf_len, true),
            21 => term_kill_region(s, 0, true),
            23 => term_kill_word_backward(s),
            25 => term_yank(s),
            27 => s.term_esc_state = IS_ESC,
            127 | 8 => term_backspace(s),
            155 => s.term_esc_state = IS_CSI,
            _ => {
                if ch >= 32 {
                    term_insert_char(s, ch);
                } else {
                    return READLINE_RET_NOT_HANDLED;
                }
            }
        },
        IS_ESC => {
            s.term_esc_state = IS_NORM;
            match ch {
                91 | 79 => {
                    s.term_esc_state = IS_CSI;
                    s.term_esc_param2 = 0;
                    s.term_esc_param1 = 0;
                    s.term_esc_param = 0;
                }
                13 => term_return(s),
                8 | 127 => term_kill_word_backward(s),
                98 => term_skip_word_backward(s),
                100 => term_kill_word(s),
                102 => term_skip_word_forward(s),
                _ => return READLINE_RET_NOT_HANDLED,
            }
        }
        IS_CSI => {
            s.term_esc_state = IS_NORM;
            match ch {
                65 => term_up_char(s),
                66 | 69 => term_down_char(s),
                68 => term_backward_char(s),
                67 => term_forward_char(s),
                70 => term_eol(s),
                72 => term_bol(s),
                59 => {
                    s.term_esc_param2 = s.term_esc_param1;
                    s.term_esc_param1 = s.term_esc_param;
                    s.term_esc_param = 0;
                    s.term_esc_state = IS_CSI;
                }
                c if (c as u8).is_ascii_digit() => {
                    s.term_esc_param = s.term_esc_param * 10 + (ch - b'0' as i32);
                    s.term_esc_state = IS_CSI;
                }
                126 => match s.term_esc_param {
                    1 => term_bol(s),
                    3 => term_delete_char(s),
                    4 => term_eol(s),
                    _ => return READLINE_RET_NOT_HANDLED,
                },
                _ => return READLINE_RET_NOT_HANDLED,
            }
        }
        _ => {}
    }
    term_update(s);
    if ret == READLINE_RET_ACCEPTED {
        term_printf!("\n");
    }
    ret
}

pub fn readline_handle_byte(s: &mut ReadlineState, c: u8) -> i32 {
    if (0xC0..0xF8).contains(&c) {
        s.utf8_state = 1 + u8::from(c >= 0xE0) + u8::from(c >= 0xF0);
        let shift = 6u32.saturating_sub(s.utf8_state as u32);
        s.utf8_val = (c as u32) & ((1u32 << shift) - 1);
        return READLINE_RET_HANDLED;
    }
    if s.utf8_state != 0 {
        if (0x80..0xC0).contains(&c) {
            s.utf8_val = (s.utf8_val << 6) | (u32::from(c) & 0x3F);
            s.utf8_state -= 1;
            if s.utf8_state != 0 {
                return READLINE_RET_HANDLED;
            }
            return readline_handle_char(s, s.utf8_val as i32);
        }
        s.utf8_state = 0;
    }
    readline_handle_char(s, c as i32)
}

pub fn readline_start(s: &mut ReadlineState, prompt: &str, is_password: bool) {
    s.term_prompt = Some(prompt.to_string());
    s.term_is_password = is_password;
    s.term_hist_entry = None;
    s.term_cmd_buf_index = 0;
    s.term_cmd_buf_len = 0;
    term_show_prompt(s);
}
