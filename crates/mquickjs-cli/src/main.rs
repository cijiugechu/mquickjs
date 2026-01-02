mod readline;
mod readline_tty;

use clap::Parser;
use crate::readline::ReadlineState;
use mquickjs::api::{
    js_dump_memory, js_eval_with_filename, js_get_error_str, js_get_global_object,
    js_is_bytecode, js_load_bytecode, js_parse_bytecode_with_filename, js_print_value,
    js_relocate_bytecode, js_run, js_set_log_func, js_set_property_str, js_prepare_bytecode,
};
#[cfg(target_pointer_width = "64")]
use mquickjs::api::js_prepare_bytecode_64to32;
use mquickjs::capi_defs::{JS_EVAL_REPL, JS_EVAL_RETVAL, JS_EVAL_STRIP_COL};
use mquickjs::builtins::js_run_timers;
use mquickjs::context::{ContextConfig, JSContext};
use mquickjs::stdlib::MQUICKJS_STDLIB_IMAGE;
use std::ffi::c_void;
use std::io::Write;
use std::path::Path;
use std::ptr::NonNull;
use std::{fs, ptr};

const DEFAULT_MEMORY_LIMIT: usize = 16 << 20;
const MAX_INCLUDE_FILES: usize = 32;
const STYLE_DEFAULT: i32 = readline::COLOR_BRIGHT_GREEN;
const STYLE_COMMENT: i32 = readline::COLOR_WHITE;
const STYLE_STRING: i32 = readline::COLOR_BRIGHT_CYAN;
const STYLE_NUMBER: i32 = readline::COLOR_GREEN;
const STYLE_KEYWORD: i32 = readline::COLOR_BRIGHT_WHITE;
const STYLE_FUNCTION: i32 = readline::COLOR_BRIGHT_YELLOW;
const STYLE_TYPE: i32 = readline::COLOR_BRIGHT_MAGENTA;
const STYLE_IDENTIFIER: i32 = readline::COLOR_BRIGHT_GREEN;
const STYLE_RESULT: i32 = readline::COLOR_BRIGHT_WHITE;
const STYLE_ERROR_MSG: i32 = readline::COLOR_BRIGHT_RED;

const JS_KEYWORDS: &str =
    "break|case|catch|continue|debugger|default|delete|do|\
else|finally|for|function|if|in|instanceof|new|\
return|switch|this|throw|try|typeof|while|with|\
class|const|enum|import|export|extends|super|\
implements|interface|let|package|private|protected|\
public|static|yield|\
undefined|null|true|false|Infinity|NaN|\
eval|arguments|\
await|";
const JS_TYPES: &str = "void|var|";

#[derive(Debug, Parser)]
#[command(name = "mquickjs-cli", disable_help_flag = true)]
struct Args {
    #[arg(short = 'h', long = "help", action = clap::ArgAction::Help)]
    _help: Option<bool>,

    #[arg(short = 'e', long = "eval")]
    eval: Option<String>,

    #[arg(short = 'i', long = "interactive", default_value_t = false)]
    interactive: bool,

    #[arg(short = 'I', long = "include")]
    includes: Vec<String>,

    #[arg(short = 'd', long = "dump", action = clap::ArgAction::Count)]
    dump: u8,

    #[arg(long = "memory-limit")]
    memory_limit: Option<String>,

    #[arg(long = "no-column", default_value_t = false)]
    no_column: bool,

    #[arg(short = 'o')]
    output: Option<String>,

    #[arg(long = "m32", default_value_t = false)]
    m32: bool,

    #[arg()]
    file_and_args: Vec<String>,
}

#[derive(Debug, Default, Eq, PartialEq)]
struct CliConfig {
    memory_limit: usize,
    dump_memory: u8,
    interactive: bool,
    eval: Option<String>,
    output: Option<String>,
    force_32bit: bool,
    parse_flags: u32,
    includes: Vec<String>,
    file: Option<String>,
    script_args: Vec<String>,
}

#[derive(Debug, Eq, PartialEq)]
enum ParseError {
    TooManyIncludes { max: usize },
    MissingMemoryLimit,
    InvalidMemoryLimit,
    MissingOutputFile,
    OutputRequiresInputFile,
    Force32RequiresOutput,
}

fn main() {
    let args = Args::parse();
    let config = match build_config(args) {
        Ok(config) => config,
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(2);
        }
    };
    if let Err(err) = run_cli(config) {
        eprintln!("{err}");
        std::process::exit(2);
    }
}

fn build_config(args: Args) -> Result<CliConfig, ParseError> {
    let mut config = CliConfig {
        memory_limit: DEFAULT_MEMORY_LIMIT,
        ..CliConfig::default()
    };

    if let Some(limit) = args.memory_limit.as_deref() {
        config.memory_limit = parse_memory_limit(limit)?;
    }

    if args.no_column {
        config.parse_flags |= JS_EVAL_STRIP_COL;
    }

    config.dump_memory = args.dump;
    config.interactive = args.interactive;
    config.eval = args.eval;
    config.output = args.output;
    config.force_32bit = args.m32;

    if args.includes.len() > MAX_INCLUDE_FILES {
        return Err(ParseError::TooManyIncludes {
            max: MAX_INCLUDE_FILES,
        });
    }
    config.includes = args.includes;

    if !args.file_and_args.is_empty() {
        config.file = Some(args.file_and_args[0].clone());
        if args.file_and_args.len() > 1 {
            config.script_args = args.file_and_args[1..].to_vec();
        }
    }

    if config.force_32bit && config.output.is_none() {
        return Err(ParseError::Force32RequiresOutput);
    }

    if config.output.is_some() && config.file.is_none() {
        return Err(ParseError::OutputRequiresInputFile);
    }

    Ok(config)
}

fn parse_memory_limit(input: &str) -> Result<usize, ParseError> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return Err(ParseError::MissingMemoryLimit);
    }

    let mut bytes = trimmed.as_bytes();
    let mut suffix = None;
    if let Some(last) = bytes.last().copied() {
        if matches!(last, b'g' | b'G' | b'm' | b'M' | b'k' | b'K') {
            suffix = Some(last);
            bytes = &bytes[..bytes.len() - 1];
        }
    }

    if bytes.is_empty() {
        return Err(ParseError::MissingMemoryLimit);
    }

    let number_str = std::str::from_utf8(bytes).map_err(|_| ParseError::InvalidMemoryLimit)?;
    let mut value: f64 = number_str.parse().map_err(|_| ParseError::InvalidMemoryLimit)?;

    let multiplier = match suffix {
        Some(b'g') | Some(b'G') => 1024.0 * 1024.0 * 1024.0,
        Some(b'm') | Some(b'M') => 1024.0 * 1024.0,
        Some(b'k') | Some(b'K') => 1024.0,
        Some(_) => 1.0,
        None => 1.0,
    };

    value *= multiplier;
    if value.is_sign_negative() || !value.is_finite() {
        return Err(ParseError::InvalidMemoryLimit);
    }

    Ok(value as usize)
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::TooManyIncludes { max } => {
                write!(f, "too many included files (max {max})")
            }
            ParseError::MissingMemoryLimit => write!(f, "expecting memory limit"),
            ParseError::InvalidMemoryLimit => write!(f, "invalid memory limit"),
            ParseError::MissingOutputFile => write!(f, "missing filename for -o"),
            ParseError::OutputRequiresInputFile => write!(f, "expecting input filename"),
            ParseError::Force32RequiresOutput => write!(f, "m32 requires -o output"),
        }
    }
}

#[derive(Debug)]
enum CliError {
    Io(String),
    Eval,
    Bytecode(String),
    Timer,
    Capi(String),
}

impl std::fmt::Display for CliError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CliError::Io(msg) => write!(f, "{msg}"),
            CliError::Eval => write!(f, "evaluation failed"),
            CliError::Bytecode(msg) => write!(f, "{msg}"),
            CliError::Timer => write!(f, "timer execution failed"),
            CliError::Capi(msg) => write!(f, "{msg}"),
        }
    }
}

fn run_cli(config: CliConfig) -> Result<(), CliError> {
    if let Some(output) = config.output.as_deref() {
        return compile_file(&config, output);
    }
    let mut ctx = create_context(&config, false).map_err(CliError::Capi)?;
    set_log_stdout(&mut ctx);
    seed_random(&mut ctx);

    for include in &config.includes {
        if eval_file(&mut ctx, include, &[], config.parse_flags)? {
            return Err(CliError::Eval);
        }
        run_timers_or_error(&mut ctx)?;
    }

    if let Some(expr) = config.eval.as_deref() {
        if eval_buf(&mut ctx, expr.as_bytes(), "<cmdline>", false, config.parse_flags | JS_EVAL_REPL)? {
            return Err(CliError::Eval);
        }
        run_timers_or_error(&mut ctx)?;
    } else if let Some(file) = config.file.as_deref() {
        if eval_file(&mut ctx, file, &config.script_args, config.parse_flags)? {
            return Err(CliError::Eval);
        }
        run_timers_or_error(&mut ctx)?;
    } else {
        return repl_run(&mut ctx, config.parse_flags, config.dump_memory);
    }

    if config.interactive {
        repl_run(&mut ctx, config.parse_flags, config.dump_memory)
    } else {
        run_timers_or_error(&mut ctx)?;
        if config.dump_memory > 0 {
            js_dump_memory(&mut ctx, config.dump_memory >= 2);
        }
        Ok(())
    }
}

fn create_context(config: &CliConfig, prepare_compilation: bool) -> Result<JSContext, String> {
    JSContext::new(ContextConfig {
        image: &MQUICKJS_STDLIB_IMAGE,
        memory_size: config.memory_limit,
        prepare_compilation,
        finalizers: &[],
    })
    .map_err(|err| format!("context init failed: {err:?}"))
}

fn set_log_stdout(ctx: &mut JSContext) {
    unsafe extern "C" fn write_stdout(
        _opaque: *mut c_void,
        buf: *const c_void,
        len: usize,
    ) {
        if buf.is_null() || len == 0 {
            return;
        }
        let slice = unsafe { std::slice::from_raw_parts(buf as *const u8, len) };
        let _ = std::io::stdout().write_all(slice);
    }
    ctx.set_opaque(ptr::null_mut());
    js_set_log_func(ctx, Some(write_stdout));
}

fn seed_random(ctx: &mut JSContext) {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let seed = ((now.as_secs() as u64) << 32) | (now.subsec_micros() as u64);
    ctx.set_random_seed(seed);
}

unsafe extern "C" fn js_interrupt_handler(
    _ctx: *mut mquickjs::capi_defs::JSContext,
    _opaque: *mut c_void,
) -> i32 {
    i32::from(readline_tty::readline_is_interrupted())
}

fn is_word(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'_' || c == b'$'
}

fn find_keyword(buf: &[u8], dict: &str) -> bool {
    for word in dict.split('|') {
        if word.is_empty() {
            continue;
        }
        if word.as_bytes() == buf {
            return true;
        }
    }
    false
}

fn buf_at(buf: &[u8], idx: usize) -> u8 {
    buf.get(idx).copied().unwrap_or(0)
}

fn term_get_color(plen: &mut i32, buf: &[u8], pos: usize, _buf_len: usize) -> i32 {
    let c = buf_at(buf, pos);
    if c == b'"' || c == b'\'' {
        let mut pos1 = pos + 1;
        loop {
            let ch = buf_at(buf, pos1);
            if ch == 0 || ch == c {
                break;
            }
            if ch == b'\\' && buf_at(buf, pos1 + 1) != 0 {
                pos1 += 2;
            } else {
                pos1 += 1;
            }
        }
        if buf_at(buf, pos1) != 0 {
            pos1 += 1;
        }
        *plen = (pos1 - pos) as i32;
        STYLE_STRING
    } else if c == b'/' && buf_at(buf, pos + 1) == b'*' {
        let mut pos1 = pos + 2;
        while buf_at(buf, pos1) != 0
            && !(buf_at(buf, pos1) == b'*' && buf_at(buf, pos1 + 1) == b'/')
        {
            pos1 += 1;
        }
        if buf_at(buf, pos1) != 0 {
            pos1 += 2;
        }
        *plen = (pos1 - pos) as i32;
        STYLE_COMMENT
    } else if c.is_ascii_digit() || c == b'.' {
        let mut pos1 = pos + 1;
        while is_word(buf_at(buf, pos1)) {
            pos1 += 1;
        }
        *plen = (pos1 - pos) as i32;
        STYLE_NUMBER
    } else if is_word(c) {
        let mut pos1 = pos + 1;
        while is_word(buf_at(buf, pos1)) {
            pos1 += 1;
        }
        let len = pos1 - pos;
        if find_keyword(&buf[pos..pos + len], JS_KEYWORDS) {
            *plen = len as i32;
            STYLE_KEYWORD
        } else {
            while buf_at(buf, pos1) == b' ' {
                pos1 += 1;
            }
            let color = if buf_at(buf, pos1) == b'(' {
                STYLE_FUNCTION
            } else if find_keyword(&buf[pos..pos + len], JS_TYPES) {
                STYLE_TYPE
            } else {
                STYLE_IDENTIFIER
            };
            *plen = len as i32;
            color
        }
    } else {
        *plen = 1;
        STYLE_DEFAULT
    }
}

fn eval_buf(
    ctx: &mut JSContext,
    eval_str: &[u8],
    filename: &str,
    is_repl: bool,
    parse_flags: u32,
) -> Result<bool, CliError> {
    let flags = if is_repl {
        parse_flags | JS_EVAL_RETVAL | JS_EVAL_REPL
    } else {
        parse_flags
    };
    let val = js_eval_with_filename(ctx, eval_str, flags, filename);
    if val.is_exception() {
        dump_error(ctx);
        return Ok(true);
    }
    if is_repl {
        ctx.write_log(readline::TERM_COLORS[STYLE_RESULT as usize].as_bytes());
        js_print_value(ctx, val);
        ctx.write_log(readline::TERM_COLORS[readline::COLOR_NONE as usize].as_bytes());
        ctx.write_log(b"\n");
    }
    Ok(false)
}

fn eval_file(
    ctx: &mut JSContext,
    filename: &str,
    script_args: &[String],
    parse_flags: u32,
) -> Result<bool, CliError> {
    let mut buf = read_file(filename)?;
    if js_is_bytecode(&buf) {
        js_relocate_bytecode(&mut buf).map_err(|err| CliError::Bytecode(format!("{err:?}")))?;
        let main_func = js_load_bytecode(ctx, &buf);
        if main_func.is_exception() {
            dump_error(ctx);
            return Ok(true);
        }
        if !script_args.is_empty() {
            set_script_args(ctx, script_args)?;
        }
        let val = js_run(ctx, main_func);
        if val.is_exception() {
            dump_error(ctx);
            return Ok(true);
        }
        return Ok(false);
    }

    if !script_args.is_empty() {
        set_script_args(ctx, script_args)?;
    }
    let val = js_eval_with_filename(ctx, &buf, parse_flags, filename);
    if val.is_exception() {
        dump_error(ctx);
        return Ok(true);
    }
    Ok(false)
}

fn set_script_args(ctx: &mut JSContext, args: &[String]) -> Result<(), CliError> {
    let global = js_get_global_object(ctx);
    let arr = mquickjs::api::js_new_array(ctx, args.len());
    for (idx, arg) in args.iter().enumerate() {
        let val = mquickjs::api::js_new_string(ctx, arg);
        let _ = mquickjs::api::js_set_property_uint32(ctx, arr, idx as u32, val);
    }
    let res = js_set_property_str(ctx, global, "scriptArgs", arr);
    if res.is_exception() {
        return Err(CliError::Capi("failed to set scriptArgs".into()));
    }
    Ok(())
}

fn run_timers_or_error(ctx: &mut JSContext) -> Result<(), CliError> {
    js_run_timers(ctx).map_err(|_| CliError::Timer)
}

fn dump_error(ctx: &mut JSContext) {
    let msg = js_get_error_str(ctx);
    let needs_newline = !msg.ends_with('\n');
    eprint!(
        "{}{}{}",
        readline::TERM_COLORS[STYLE_ERROR_MSG as usize],
        msg,
        readline::TERM_COLORS[readline::COLOR_NONE as usize]
    );
    if needs_newline {
        eprintln!();
    }
}

fn compile_file(config: &CliConfig, output: &str) -> Result<(), CliError> {
    let filename = config
        .file
        .as_deref()
        .ok_or_else(|| CliError::Bytecode("expecting input filename".into()))?;
    let mut ctx = create_context(config, true).map_err(CliError::Capi)?;
    set_log_stdout(&mut ctx);
    let buf = read_file(filename)?;
    let func = js_parse_bytecode_with_filename(&mut ctx, &buf, config.parse_flags, filename);
    if func.is_exception() {
        dump_error(&mut ctx);
        return Err(CliError::Eval);
    }
    let out = if config.force_32bit {
        #[cfg(target_pointer_width = "64")]
        {
            js_prepare_bytecode_64to32(&mut ctx, func)
                .map_err(|err| CliError::Bytecode(err.to_string()))?
        }
        #[cfg(not(target_pointer_width = "64"))]
        {
            return Err(CliError::Bytecode("m32 requires 64-bit host".into()));
        }
    } else {
        js_prepare_bytecode(&mut ctx, func).map_err(|err| CliError::Bytecode(err.to_string()))?
    };
    fs::write(output, out).map_err(|err| CliError::Io(err.to_string()))?;
    if config.dump_memory > 0 {
        js_dump_memory(&mut ctx, config.dump_memory >= 2);
    }
    Ok(())
}

fn read_file(path: &str) -> Result<Vec<u8>, CliError> {
    fs::read(Path::new(path)).map_err(|err| CliError::Io(format!("{path}: {err}")))
}

fn repl_run(ctx: &mut JSContext, parse_flags: u32, dump_memory: u8) -> Result<(), CliError> {
    let mut readline = Readline::new();
    readline.init_terminal();
    ctx.set_interrupt_handler(Some(js_interrupt_handler));
    loop {
        let cmd = readline.readline("mqjs > ");
        let Some(cmd) = cmd else {
            break;
        };
        let _ = eval_buf(ctx, &cmd, "<cmdline>", true, parse_flags)?;
        run_timers_or_error(ctx)?;
    }
    if dump_memory > 0 {
        js_dump_memory(ctx, dump_memory >= 2);
    }
    Ok(())
}

struct Readline {
    state: ReadlineState,
    cmd_buf: [u8; 256],
    kill_buf: [u8; 256],
    history: [u8; 512],
}

impl Readline {
    fn new() -> Self {
        let mut state = ReadlineState::default();
        state.term_cmd_buf_size = 256;
        state.term_kill_buf_len = 0;
        state.term_history_buf_size = 512;
        Self {
            state,
            cmd_buf: [0u8; 256],
            kill_buf: [0u8; 256],
            history: [0u8; 512],
        }
    }

    fn init_terminal(&mut self) {
        self.state.term_width = readline_tty::readline_tty_init();
        self.state.term_cmd_buf = NonNull::new(self.cmd_buf.as_mut_ptr());
        self.state.term_kill_buf = NonNull::new(self.kill_buf.as_mut_ptr());
        self.state.term_history = NonNull::new(self.history.as_mut_ptr());
        self.state.get_color = Some(term_get_color);
    }

    fn readline(&mut self, prompt: &str) -> Option<Vec<u8>> {
        readline_tty::readline_tty(&mut self.state, prompt, false)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn args_from<I, S>(input: I) -> Args
    where
        I: IntoIterator<Item = S>,
        S: Into<std::ffi::OsString> + Clone,
    {
        Args::parse_from(input)
    }

    #[test]
    fn memory_limit_defaults_to_16m() {
        let args = args_from(["mqjs"]);
        let config = build_config(args).expect("config");
        assert_eq!(config.memory_limit, DEFAULT_MEMORY_LIMIT);
    }

    #[test]
    fn memory_limit_parses_suffixes() {
        let args = args_from(["mqjs", "--memory-limit", "2k"]);
        let config = build_config(args).expect("config");
        assert_eq!(config.memory_limit, 2 * 1024);

        let args = args_from(["mqjs", "--memory-limit", "1m"]);
        let config = build_config(args).expect("config");
        assert_eq!(config.memory_limit, 1024 * 1024);

        let args = args_from(["mqjs", "--memory-limit", "1.5g"]);
        let config = build_config(args).expect("config");
        assert_eq!(config.memory_limit, (1.5 * 1024.0 * 1024.0 * 1024.0) as usize);
    }

    #[test]
    fn no_column_sets_parse_flag() {
        let args = args_from(["mqjs", "--no-column"]);
        let config = build_config(args).expect("config");
        assert_eq!(config.parse_flags & JS_EVAL_STRIP_COL, JS_EVAL_STRIP_COL);
    }

    #[test]
    fn includes_are_limited() {
        let mut argv = vec!["mqjs".to_string()];
        for idx in 0..(MAX_INCLUDE_FILES + 1) {
            argv.push("-I".into());
            argv.push(format!("file{idx}.js"));
        }
        let args = args_from(argv);
        let err = build_config(args).expect_err("error");
        assert_eq!(err, ParseError::TooManyIncludes { max: MAX_INCLUDE_FILES });
    }

    #[test]
    fn output_requires_input_file() {
        let args = args_from(["mqjs", "-o", "out.bin"]);
        let err = build_config(args).expect_err("error");
        assert_eq!(err, ParseError::OutputRequiresInputFile);
    }

    #[test]
    fn force_32_requires_output() {
        let args = args_from(["mqjs", "--m32", "input.js"]);
        let err = build_config(args).expect_err("error");
        assert_eq!(err, ParseError::Force32RequiresOutput);
    }

    #[test]
    fn file_and_script_args_are_captured() {
        let args = args_from(["mqjs", "script.js", "a", "b"]);
        let config = build_config(args).expect("config");
        assert_eq!(config.file.as_deref(), Some("script.js"));
        assert_eq!(config.script_args, vec!["a", "b"]);
    }

    #[test]
    fn term_get_color_identifies_keywords() {
        let mut len = 0;
        let buf = b"return value";
        let color = term_get_color(&mut len, buf, 0, buf.len());
        assert_eq!(color, STYLE_KEYWORD);
        assert_eq!(len, "return".len() as i32);
    }

    #[test]
    fn term_get_color_identifies_function_calls() {
        let mut len = 0;
        let buf = b"foo (1)";
        let color = term_get_color(&mut len, buf, 0, buf.len());
        assert_eq!(color, STYLE_FUNCTION);
        assert_eq!(len, "foo".len() as i32);
    }

    #[test]
    fn term_get_color_identifies_strings_and_numbers() {
        let mut len = 0;
        let buf = br#""abc" 42"#;
        let color = term_get_color(&mut len, buf, 0, buf.len());
        assert_eq!(color, STYLE_STRING);
        assert_eq!(len, "\"abc\"".len() as i32);

        let color = term_get_color(&mut len, buf, 6, buf.len());
        assert_eq!(color, STYLE_NUMBER);
        assert_eq!(len, "42".len() as i32);
    }
}
