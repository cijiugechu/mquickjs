use crate::capi_defs::{
    JS_EVAL_JSON, JS_EVAL_REGEXP, JS_EVAL_REGEXP_FLAGS_SHIFT, JS_EVAL_REPL, JS_EVAL_RETVAL,
    JS_EVAL_STRIP_COL,
};
use crate::context::JSContext;
use crate::parser::expr::ExprParser;
use crate::parser::json::{parse_json, JsonError, JsonValue};
use crate::parser::pos::get_line_col;
use crate::parser::regexp::{compile_regexp, RegExpBytecode, RegExpError};
use crate::parser::error::ParserError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError {
    message: String,
    position: usize,
    line: i32,
    column: i32,
}

impl ParseError {
    fn new(message: String, position: usize, source: &[u8]) -> Self {
        let (line, column) = line_col(source, position);
        Self {
            message,
            position,
            line,
            column,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn position(&self) -> usize {
        self.position
    }

    pub fn line(&self) -> i32 {
        self.line
    }

    pub fn column(&self) -> i32 {
        self.column
    }
}

pub enum ParseOutput<'a, 'ctx> {
    Program(Box<ExprParser<'a, 'ctx>>),
    Json(JsonValue),
    RegExp(RegExpBytecode),
}

pub fn parse_source<'a, 'ctx>(
    ctx: &'ctx mut JSContext,
    source: &'a [u8],
    eval_flags: u32,
) -> Result<ParseOutput<'a, 'ctx>, ParseError> {
    if (eval_flags & JS_EVAL_JSON) != 0 {
        parse_json(source)
            .map(ParseOutput::Json)
            .map_err(|err| parse_json_error(err, source))
    } else if (eval_flags & JS_EVAL_REGEXP) != 0 {
        let re_flags = eval_flags >> JS_EVAL_REGEXP_FLAGS_SHIFT;
        compile_regexp(source, re_flags)
            .map(ParseOutput::RegExp)
            .map_err(|err| parse_regexp_error(err, source))
    } else {
        let mut parser = Box::new(ExprParser::new(ctx, source));
        let has_column = (eval_flags & JS_EVAL_STRIP_COL) == 0;
        let has_retval = (eval_flags & JS_EVAL_RETVAL) != 0;
        let is_repl = (eval_flags & JS_EVAL_REPL) != 0;
        parser.set_has_column(has_column);
        parser.attach_parse_state();
        parser
            .parse_program_with_flags(true, has_retval, is_repl)
            .map_err(|err| parse_js_error(err, source))?;
        Ok(ParseOutput::Program(parser))
    }
}

fn line_col(source: &[u8], position: usize) -> (i32, i32) {
    let pos = position.min(source.len());
    get_line_col(&source[..pos])
}

fn parse_js_error(err: ParserError, source: &[u8]) -> ParseError {
    ParseError::new(err.message().into_owned(), err.position(), source)
}

fn parse_json_error(err: JsonError, source: &[u8]) -> ParseError {
    ParseError::new(err.message().to_string(), err.position(), source)
}

fn parse_regexp_error(err: RegExpError, source: &[u8]) -> ParseError {
    ParseError::new(err.message().to_string(), err.position(), source)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::capi_defs::{
        JS_EVAL_JSON, JS_EVAL_REGEXP, JS_EVAL_REGEXP_FLAGS_SHIFT, JS_EVAL_STRIP_COL,
    };
    use crate::context::{ContextConfig, JSContext};
    use crate::parser::regexp_flags::LRE_FLAG_STICKY;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
        })
        .expect("context init")
    }

    #[test]
    fn parse_source_program_has_column_flag() {
        let mut ctx = new_context();
        let output = parse_source(&mut ctx, b"1", 0).expect("parse");
        let ParseOutput::Program(parser) = output else {
            panic!("expected program");
        };
        assert!(parser.has_column());

        let mut ctx = new_context();
        let output = parse_source(&mut ctx, b"1", JS_EVAL_STRIP_COL).expect("parse");
        let ParseOutput::Program(parser) = output else {
            panic!("expected program");
        };
        assert!(!parser.has_column());
    }

    #[test]
    fn parse_source_program_error_line_col() {
        let input = b"var x = 1;\nreturn x;";
        let mut ctx = new_context();
        let err = match parse_source(&mut ctx, input, 0) {
            Ok(_) => panic!("expected parse error"),
            Err(err) => err,
        };
        assert_eq!(err.message(), "return not in a function");
        assert_eq!(err.position(), 11);
        assert_eq!(err.line(), 1);
        assert_eq!(err.column(), 0);
    }

    #[test]
    fn parse_source_json_error_line_col() {
        let input = b"{\n\"a\": }";
        let mut ctx = new_context();
        let err = match parse_source(&mut ctx, input, JS_EVAL_JSON) {
            Ok(_) => panic!("expected parse error"),
            Err(err) => err,
        };
        assert_eq!(err.message(), "unexpected character");
        assert_eq!(err.position(), 7);
        assert_eq!(err.line(), 1);
        assert_eq!(err.column(), 5);
    }

    #[test]
    fn parse_source_regexp_flags() {
        let eval_flags =
            JS_EVAL_REGEXP | (LRE_FLAG_STICKY << JS_EVAL_REGEXP_FLAGS_SHIFT);
        let mut ctx = new_context();
        let output = parse_source(&mut ctx, b"a+", eval_flags).expect("parse");
        let ParseOutput::RegExp(bytecode) = output else {
            panic!("expected regexp");
        };
        assert_eq!(bytecode.flags() as u32, LRE_FLAG_STICKY);
    }
}
