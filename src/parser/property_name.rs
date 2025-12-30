use crate::dtoa::{js_dtoa, JS_DTOA_EXP_AUTO, JS_DTOA_FORMAT_FREE};
use crate::jsvalue::JSValue;

use super::error::{ParserError, ParserErrorKind, ERR_NO_MEM};
use super::lexer::{value_matches_bytes, ParseState};
use super::parse_state::ParseProp;
use super::tokens::{TOK_FIRST_KEYWORD, TOK_IDENT, TOK_NUMBER, TOK_STRING};
use super::types::TokenExtra;

const ERR_INVALID_PROPERTY_NAME: &str = "invalid property name";
const TOK_PAREN_OPEN: i32 = b'(' as i32;

// C: js_parse_property_name in mquickjs.c.
pub fn parse_property_name(state: &mut ParseState) -> Result<(ParseProp, JSValue), ParserError> {
    let mut prop_type = ParseProp::Field;
    let mut token = state.token();

    if token.val() == TOK_IDENT {
        let ident_value = token.value();
        let is_set = if value_matches_bytes(ident_value, b"get") {
            Some(false)
        } else if value_matches_bytes(ident_value, b"set") {
            Some(true)
        } else {
            None
        };
        if let Some(is_set) = is_set {
            state.next_token().map_err(ParserError::from)?;
            token = state.token();
            let val = token.val();
            if val == b':' as i32
                || val == b',' as i32
                || val == b'}' as i32
                || val == TOK_PAREN_OPEN
            {
                let name = state.atomize_value(ident_value);
                if token.val() == TOK_PAREN_OPEN {
                    prop_type = ParseProp::Method;
                }
                return Ok((prop_type, name));
            }
            prop_type = if is_set { ParseProp::Set } else { ParseProp::Get };
        }
    }

    let name = if token.val() == TOK_NUMBER {
        let number = match token.extra() {
            TokenExtra::Number(value) => value,
            _ => {
                return Err(ParserError::new(
                    ParserErrorKind::Static(ERR_INVALID_PROPERTY_NAME),
                    token.source_pos() as usize,
                ));
            }
        };
        number_to_property_key(state, number, token.source_pos() as usize)?
    } else if token.val() == TOK_IDENT
        || token.val() >= TOK_FIRST_KEYWORD
        || token.val() == TOK_STRING
    {
        token.value()
    } else {
        return Err(ParserError::new(
            ParserErrorKind::Static(ERR_INVALID_PROPERTY_NAME),
            token.source_pos() as usize,
        ));
    };

    let name = state.atomize_value(name);
    state.next_token().map_err(ParserError::from)?;
    if prop_type == ParseProp::Field && state.token().val() == TOK_PAREN_OPEN {
        prop_type = ParseProp::Method;
    }
    Ok((prop_type, name))
}

fn number_to_property_key(
    state: &mut ParseState,
    number: f64,
    err_pos: usize,
) -> Result<JSValue, ParserError> {
    let text = js_dtoa(number, 10, 0, JS_DTOA_FORMAT_FREE | JS_DTOA_EXP_AUTO).map_err(|_| {
        ParserError::new(ParserErrorKind::Static(ERR_NO_MEM), err_pos)
    })?;
    state
        .value_from_bytes(text.into_bytes(), true, false, err_pos)
        .map_err(ParserError::from)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::context::{ContextConfig, JSContext};
    use crate::string::runtime::string_view;
    use crate::stdlib::MQUICKJS_STDLIB_IMAGE;

    fn value_bytes(value: JSValue) -> Vec<u8> {
        let mut scratch = [0u8; 5];
        let view = string_view(value, &mut scratch).expect("string view");
        view.bytes().to_vec()
    }

    fn new_context() -> JSContext {
        JSContext::new(ContextConfig {
            image: &MQUICKJS_STDLIB_IMAGE,
            memory_size: 16 * 1024,
            prepare_compilation: false,
            finalizers: &[],
        })
        .expect("context init")
    }

    fn parse_name(input: &str) -> (ParseProp, Vec<u8>, i32) {
        let mut ctx = new_context();
        let mut state = ParseState::new(input.as_bytes(), &mut ctx);
        state.next_token().expect("next token");
        let (prop, name) = parse_property_name(&mut state).expect("property name");
        let bytes = value_bytes(name);
        (prop, bytes, state.token().val())
    }

    #[test]
    fn property_name_get_accessor() {
        let (prop, name, next) = parse_name("get foo(");
        assert_eq!(prop, ParseProp::Get);
        assert_eq!(name, b"foo");
        assert_eq!(next, TOK_PAREN_OPEN);
    }

    #[test]
    fn property_name_set_accessor() {
        let (prop, name, next) = parse_name("set foo(");
        assert_eq!(prop, ParseProp::Set);
        assert_eq!(name, b"foo");
        assert_eq!(next, TOK_PAREN_OPEN);
    }

    #[test]
    fn property_name_get_field() {
        let (prop, name, next) = parse_name("get:");
        assert_eq!(prop, ParseProp::Field);
        assert_eq!(name, b"get");
        assert_eq!(next, b':' as i32);
    }

    #[test]
    fn property_name_method_detection() {
        let (prop, name, next) = parse_name("foo(");
        assert_eq!(prop, ParseProp::Method);
        assert_eq!(name, b"foo");
        assert_eq!(next, TOK_PAREN_OPEN);
    }

    #[test]
    fn property_name_number() {
        let (prop, name, next) = parse_name("1:");
        assert_eq!(prop, ParseProp::Field);
        assert_eq!(name, b"1");
        assert_eq!(next, b':' as i32);
    }

    #[test]
    fn property_name_keyword() {
        let (prop, name, next) = parse_name("if:");
        assert_eq!(prop, ParseProp::Field);
        assert_eq!(name, b"if");
        assert_eq!(next, b':' as i32);
    }

    #[test]
    fn property_name_string() {
        let (prop, name, next) = parse_name("\"bar\":");
        assert_eq!(prop, ParseProp::Field);
        assert_eq!(name, b"bar");
        assert_eq!(next, b':' as i32);
    }
}