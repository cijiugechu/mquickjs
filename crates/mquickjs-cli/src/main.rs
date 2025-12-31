use clap::Parser;
use mquickjs::capi_defs::JS_EVAL_STRIP_COL;

const DEFAULT_MEMORY_LIMIT: usize = 16 << 20;
const MAX_INCLUDE_FILES: usize = 32;

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
    if let Err(err) = build_config(args) {
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
}
