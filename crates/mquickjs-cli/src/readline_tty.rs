use crate::readline::{
    readline_handle_byte, readline_start, ReadlineState, READLINE_RET_ACCEPTED,
    READLINE_RET_EXIT,
};
use std::io::{self, Write};
use std::sync::atomic::{AtomicUsize, Ordering};

static CTRL_C_PRESSED: AtomicUsize = AtomicUsize::new(0);

#[cfg(unix)]
mod unix {
    use super::*;
    use std::mem::MaybeUninit;
    use std::sync::Once;

    static INIT: Once = Once::new();
    static mut OLD_TTY: MaybeUninit<libc::termios> = MaybeUninit::uninit();
    static mut OLD_TTY_SET: bool = false;
    static mut OLD_FD0_FLAGS: libc::c_int = 0;

    unsafe extern "C" fn term_exit() {
        if !OLD_TTY_SET {
            return;
        }
        let old = OLD_TTY.assume_init_ref();
        libc::tcsetattr(0, libc::TCSANOW, old);
        libc::fcntl(0, libc::F_SETFL, OLD_FD0_FLAGS);
    }

    extern "C" fn sigint_handler(_signo: libc::c_int) {
        let count = CTRL_C_PRESSED.fetch_add(1, Ordering::Relaxed) + 1;
        if count >= 4 {
            unsafe {
                libc::signal(libc::SIGINT, libc::SIG_DFL);
            }
        }
    }

    pub(super) fn readline_tty_init() -> i32 {
        unsafe {
            let mut tty = MaybeUninit::<libc::termios>::uninit();
            if libc::tcgetattr(0, tty.as_mut_ptr()) != 0 {
                return 80;
            }
            let mut tty = tty.assume_init();
            INIT.call_once(|| {
                let mut old = MaybeUninit::<libc::termios>::uninit();
                if libc::tcgetattr(0, old.as_mut_ptr()) == 0 {
                    OLD_TTY.write(old.assume_init());
                    OLD_TTY_SET = true;
                    OLD_FD0_FLAGS = libc::fcntl(0, libc::F_GETFL);
                    libc::atexit(term_exit);
                }
                libc::signal(libc::SIGINT, sigint_handler as libc::sighandler_t);
            });

            tty.c_iflag &=
                !(libc::IGNBRK | libc::BRKINT | libc::PARMRK | libc::ISTRIP | libc::INLCR
                    | libc::IGNCR | libc::ICRNL | libc::IXON);
            tty.c_oflag |= libc::OPOST;
            tty.c_lflag &= !(libc::ECHO | libc::ECHONL | libc::ICANON | libc::IEXTEN);
            tty.c_cflag &= !(libc::CSIZE | libc::PARENB);
            tty.c_cflag |= libc::CS8;
            tty.c_cc[libc::VMIN] = 1;
            tty.c_cc[libc::VTIME] = 0;

            libc::tcsetattr(0, libc::TCSANOW, &tty);

            let mut n_cols = 80;
            let mut ws = MaybeUninit::<libc::winsize>::uninit();
            if libc::ioctl(0, libc::TIOCGWINSZ, ws.as_mut_ptr()) == 0 {
                let ws = ws.assume_init();
                if ws.ws_col >= 4 && ws.ws_row >= 4 {
                    n_cols = ws.ws_col as i32;
                }
            }
            n_cols
        }
    }

    pub(super) fn readline_tty(
        s: &mut ReadlineState,
        prompt: &str,
        _multi_line: bool,
    ) -> Option<Vec<u8>> {
        readline_start(s, prompt, false);
        let mut ctrl_c_count = 0;
        let mut buf = [0u8; 128];

        loop {
            let len = unsafe {
                libc::read(
                    0,
                    buf.as_mut_ptr().cast::<libc::c_void>(),
                    buf.len(),
                )
            };
            if len == 0 {
                break;
            }
            if len < 0 {
                let err = io::Error::last_os_error();
                if err.kind() == io::ErrorKind::Interrupted {
                    continue;
                }
                break;
            }
            for &byte in &buf[..len as usize] {
                let ret = readline_handle_byte(s, byte);
                if ret == READLINE_RET_EXIT {
                    return None;
                }
                if ret == READLINE_RET_ACCEPTED {
                    return Some(s.cmd_buf_slice().to_vec());
                }
                ctrl_c_count = 0;
            }
            if CTRL_C_PRESSED.swap(0, Ordering::Relaxed) > 0 {
                if ctrl_c_count == 0 {
                    println!("(Press Ctrl-C again to quit)");
                    let _ = io::stdout().flush();
                    ctrl_c_count += 1;
                } else {
                    println!("Exiting.");
                    let _ = io::stdout().flush();
                    break;
                }
            }
        }
        None
    }

    pub(super) fn readline_is_interrupted() -> bool {
        CTRL_C_PRESSED.swap(0, Ordering::Relaxed) != 0
    }
}

#[cfg(windows)]
mod windows {
    use super::*;

    pub(super) fn readline_tty_init() -> i32 {
        80
    }

    pub(super) fn readline_tty(
        _s: &mut ReadlineState,
        prompt: &str,
        _multi_line: bool,
    ) -> Option<Vec<u8>> {
        let _ = io::stdout().write_all(prompt.as_bytes());
        let _ = io::stdout().flush();
        let mut line = String::new();
        let read = io::stdin().read_line(&mut line).ok()?;
        if read == 0 {
            return None;
        }
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        Some(line.into_bytes())
    }

    pub(super) fn readline_is_interrupted() -> bool {
        false
    }
}

#[cfg(unix)]
pub use unix::{readline_is_interrupted, readline_tty, readline_tty_init};

#[cfg(windows)]
pub use windows::{readline_is_interrupted, readline_tty, readline_tty_init};
