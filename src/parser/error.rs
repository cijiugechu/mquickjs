use std::borrow::Cow;
use std::fmt;

use crate::parser::lexer;
use crate::parser::tokens::TOK_EOF;
use crate::parser::types::Token;

pub const ERR_NO_MEM: &str = "not enough memory";
pub const ERR_STACK_OVERFLOW: &str = "stack overflow";

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParserErrorKind {
    Static(&'static str),
    ExpectingChar(u8),
}

#[derive(Clone, PartialEq, Eq)]
pub struct ParserError {
    kind: ParserErrorKind,
    position: usize,
}

impl ParserError {
    pub fn new(kind: ParserErrorKind, position: usize) -> Self {
        Self { kind, position }
    }

    pub fn kind(&self) -> ParserErrorKind {
        self.kind
    }

    pub fn message(&self) -> Cow<'static, str> {
        match self.kind {
            ParserErrorKind::Static(message) => Cow::Borrowed(message),
            ParserErrorKind::ExpectingChar(ch) => {
                Cow::Owned(format!("expecting '{}'", ch as char))
            }
        }
    }

    pub fn position(&self) -> usize {
        self.position
    }
}

impl fmt::Debug for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ParserError {{ message: \"{}\", position: {} }}",
            self,
            self.position
        )
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            ParserErrorKind::Static(message) => f.write_str(message),
            ParserErrorKind::ExpectingChar(ch) => write!(f, "expecting '{}'", ch as char),
        }
    }
}

impl From<lexer::ParseError> for ParserError {
    fn from(err: lexer::ParseError) -> Self {
        Self::new(ParserErrorKind::Static(err.message()), err.position())
    }
}

pub trait TokenStream {
    fn token(&self) -> Token;
    fn got_lf(&self) -> bool;
    fn next_token(&mut self) -> Result<(), ParserError>;
}

impl<'a> TokenStream for lexer::ParseState<'a> {
    fn token(&self) -> Token {
        self.token()
    }

    fn got_lf(&self) -> bool {
        self.got_lf()
    }

    fn next_token(&mut self) -> Result<(), ParserError> {
        self.next_token().map_err(ParserError::from)
    }
}

pub fn parse_expect1<S: TokenStream>(state: &S, ch: i32) -> Result<(), ParserError> {
    let token = state.token();
    if token.val() != ch {
        let pos = token.source_pos() as usize;
        return Err(ParserError::new(
            ParserErrorKind::ExpectingChar(ch as u8),
            pos,
        ));
    }
    Ok(())
}

pub fn parse_expect<S: TokenStream>(state: &mut S, ch: i32) -> Result<(), ParserError> {
    parse_expect1(state, ch)?;
    state.next_token()
}

pub fn parse_expect_semi<S: TokenStream>(state: &mut S) -> Result<(), ParserError> {
    let token = state.token();
    if token.val() != ';' as i32 {
        if token.val() == TOK_EOF || token.val() == '}' as i32 || state.got_lf() {
            return Ok(());
        }
        let pos = token.source_pos() as usize;
        return Err(ParserError::new(ParserErrorKind::ExpectingChar(b';'), pos));
    }
    state.next_token()
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;
    use crate::jsvalue::JS_NULL;
    use crate::parser::types::TokenExtra;

    #[derive(Debug)]
    struct DummyState {
        token: Token,
        got_lf: bool,
        next_calls: usize,
    }

    impl DummyState {
        fn new(val: i32, source_pos: u32, got_lf: bool) -> Self {
            Self {
                token: Token::new(val, source_pos, TokenExtra::None, JS_NULL),
                got_lf,
                next_calls: 0,
            }
        }
    }

    impl TokenStream for DummyState {
        fn token(&self) -> Token {
            self.token
        }

        fn got_lf(&self) -> bool {
            self.got_lf
        }

        fn next_token(&mut self) -> Result<(), ParserError> {
            self.next_calls += 1;
            Ok(())
        }
    }

    #[test]
    fn expect1_reports_mismatch_at_token_pos() {
        let state = DummyState::new('(' as i32, 42, false);
        let err = parse_expect1(&state, ';' as i32).unwrap_err();
        assert_eq!(err.message(), "expecting ';'");
        assert_eq!(err.position(), 42);
    }

    #[test]
    fn expect_consumes_next_token() {
        let mut state = DummyState::new(';' as i32, 7, false);
        parse_expect(&mut state, ';' as i32).unwrap();
        assert_eq!(state.next_calls, 1);
    }

    #[test]
    fn expect_semi_allows_asi_cases() {
        let mut eof_state = DummyState::new(TOK_EOF, 1, false);
        parse_expect_semi(&mut eof_state).unwrap();
        assert_eq!(eof_state.next_calls, 0);

        let mut rbrace_state = DummyState::new('}' as i32, 2, false);
        parse_expect_semi(&mut rbrace_state).unwrap();
        assert_eq!(rbrace_state.next_calls, 0);

        let mut lf_state = DummyState::new('+' as i32, 3, true);
        parse_expect_semi(&mut lf_state).unwrap();
        assert_eq!(lf_state.next_calls, 0);
    }

    #[test]
    fn expect_semi_consumes_explicit_semicolon() {
        let mut state = DummyState::new(';' as i32, 5, false);
        parse_expect_semi(&mut state).unwrap();
        assert_eq!(state.next_calls, 1);
    }

    #[test]
    fn expect_semi_errors_without_asi() {
        let mut state = DummyState::new('+' as i32, 9, false);
        let err = parse_expect_semi(&mut state).unwrap_err();
        assert_eq!(err.message(), "expecting ';'");
        assert_eq!(err.position(), 9);
        assert_eq!(state.next_calls, 0);
    }
}
