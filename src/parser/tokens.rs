// C: token/keyword constants and helpers from mquickjs.c.

#[repr(i32)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum KeywordAtom {
    Null = 0,
    False,
    True,
    If,
    Else,
    Return,
    Var,
    This,
    Delete,
    Void,
    Typeof,
    New,
    In,
    Instanceof,
    Do,
    While,
    For,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Throw,
    Try,
    Catch,
    Finally,
    Function,
    Debugger,
    With,
    Class,
    Const,
    Enum,
    Export,
    Extends,
    Import,
    Super,
    Implements,
    Interface,
    Let,
    Package,
    Private,
    Protected,
    Public,
    Static,
    Yield,
}

pub const TOK_NUMBER: i32 = 128;
pub const TOK_STRING: i32 = TOK_NUMBER + 1;
pub const TOK_IDENT: i32 = TOK_STRING + 1;
pub const TOK_REGEXP: i32 = TOK_IDENT + 1;

// Warning: order matters (see js_parse_assign_expr in C).
pub const TOK_MUL_ASSIGN: i32 = TOK_REGEXP + 1;
pub const TOK_DIV_ASSIGN: i32 = TOK_MUL_ASSIGN + 1;
pub const TOK_MOD_ASSIGN: i32 = TOK_DIV_ASSIGN + 1;
pub const TOK_PLUS_ASSIGN: i32 = TOK_MOD_ASSIGN + 1;
pub const TOK_MINUS_ASSIGN: i32 = TOK_PLUS_ASSIGN + 1;
pub const TOK_SHL_ASSIGN: i32 = TOK_MINUS_ASSIGN + 1;
pub const TOK_SAR_ASSIGN: i32 = TOK_SHL_ASSIGN + 1;
pub const TOK_SHR_ASSIGN: i32 = TOK_SAR_ASSIGN + 1;
pub const TOK_AND_ASSIGN: i32 = TOK_SHR_ASSIGN + 1;
pub const TOK_XOR_ASSIGN: i32 = TOK_AND_ASSIGN + 1;
pub const TOK_OR_ASSIGN: i32 = TOK_XOR_ASSIGN + 1;
pub const TOK_POW_ASSIGN: i32 = TOK_OR_ASSIGN + 1;
pub const TOK_DEC: i32 = TOK_POW_ASSIGN + 1;
pub const TOK_INC: i32 = TOK_DEC + 1;
pub const TOK_SHL: i32 = TOK_INC + 1;
pub const TOK_SAR: i32 = TOK_SHL + 1;
pub const TOK_SHR: i32 = TOK_SAR + 1;
pub const TOK_LT: i32 = TOK_SHR + 1;
pub const TOK_LTE: i32 = TOK_LT + 1;
pub const TOK_GT: i32 = TOK_LTE + 1;
pub const TOK_GTE: i32 = TOK_GT + 1;
pub const TOK_EQ: i32 = TOK_GTE + 1;
pub const TOK_STRICT_EQ: i32 = TOK_EQ + 1;
pub const TOK_NEQ: i32 = TOK_STRICT_EQ + 1;
pub const TOK_STRICT_NEQ: i32 = TOK_NEQ + 1;
pub const TOK_LAND: i32 = TOK_STRICT_NEQ + 1;
pub const TOK_LOR: i32 = TOK_LAND + 1;
pub const TOK_POW: i32 = TOK_LOR + 1;
pub const TOK_EOF: i32 = TOK_POW + 1;

pub const TOK_FIRST_KEYWORD: i32 = TOK_EOF + 1;
pub const TOK_NULL: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Null as i32;
pub const TOK_FALSE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::False as i32;
pub const TOK_TRUE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::True as i32;
pub const TOK_IF: i32 = TOK_FIRST_KEYWORD + KeywordAtom::If as i32;
pub const TOK_ELSE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Else as i32;
pub const TOK_RETURN: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Return as i32;
pub const TOK_VAR: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Var as i32;
pub const TOK_THIS: i32 = TOK_FIRST_KEYWORD + KeywordAtom::This as i32;
pub const TOK_DELETE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Delete as i32;
pub const TOK_VOID: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Void as i32;
pub const TOK_TYPEOF: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Typeof as i32;
pub const TOK_NEW: i32 = TOK_FIRST_KEYWORD + KeywordAtom::New as i32;
pub const TOK_IN: i32 = TOK_FIRST_KEYWORD + KeywordAtom::In as i32;
pub const TOK_INSTANCEOF: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Instanceof as i32;
pub const TOK_DO: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Do as i32;
pub const TOK_WHILE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::While as i32;
pub const TOK_FOR: i32 = TOK_FIRST_KEYWORD + KeywordAtom::For as i32;
pub const TOK_BREAK: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Break as i32;
pub const TOK_CONTINUE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Continue as i32;
pub const TOK_SWITCH: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Switch as i32;
pub const TOK_CASE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Case as i32;
pub const TOK_DEFAULT: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Default as i32;
pub const TOK_THROW: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Throw as i32;
pub const TOK_TRY: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Try as i32;
pub const TOK_CATCH: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Catch as i32;
pub const TOK_FINALLY: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Finally as i32;
pub const TOK_FUNCTION: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Function as i32;
pub const TOK_DEBUGGER: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Debugger as i32;
pub const TOK_WITH: i32 = TOK_FIRST_KEYWORD + KeywordAtom::With as i32;
pub const TOK_CLASS: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Class as i32;
pub const TOK_CONST: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Const as i32;
pub const TOK_ENUM: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Enum as i32;
pub const TOK_EXPORT: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Export as i32;
pub const TOK_EXTENDS: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Extends as i32;
pub const TOK_IMPORT: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Import as i32;
pub const TOK_SUPER: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Super as i32;
pub const TOK_IMPLEMENTS: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Implements as i32;
pub const TOK_INTERFACE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Interface as i32;
pub const TOK_LET: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Let as i32;
pub const TOK_PACKAGE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Package as i32;
pub const TOK_PRIVATE: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Private as i32;
pub const TOK_PROTECTED: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Protected as i32;
pub const TOK_PUBLIC: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Public as i32;
pub const TOK_STATIC: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Static as i32;
pub const TOK_YIELD: i32 = TOK_FIRST_KEYWORD + KeywordAtom::Yield as i32;

const TOK_RPAREN: i32 = b')' as i32;
const TOK_RBRACKET: i32 = b']' as i32;

// C: is_regexp_allowed in mquickjs.c.
pub fn is_regexp_allowed(tok: i32) -> bool {
    !matches!(
        tok,
        TOK_NUMBER
            | TOK_STRING
            | TOK_REGEXP
            | TOK_DEC
            | TOK_INC
            | TOK_NULL
            | TOK_FALSE
            | TOK_TRUE
            | TOK_THIS
            | TOK_IF
            | TOK_WHILE
            | TOK_FOR
            | TOK_DO
            | TOK_CASE
            | TOK_CATCH
            | TOK_IDENT
            | TOK_RPAREN
            | TOK_RBRACKET
    )
}

#[cfg(all(test, not(miri)))]
mod tests {
    use super::*;

    #[test]
    fn assign_token_ordering_is_contiguous() {
        assert_eq!(TOK_OR_ASSIGN, TOK_MUL_ASSIGN + 10);
        assert_eq!(TOK_POW_ASSIGN, TOK_OR_ASSIGN + 1);
    }

    #[test]
    fn keyword_tokens_follow_atom_order() {
        let tokens = [
            TOK_NULL,
            TOK_FALSE,
            TOK_TRUE,
            TOK_IF,
            TOK_ELSE,
            TOK_RETURN,
            TOK_VAR,
            TOK_THIS,
            TOK_DELETE,
            TOK_VOID,
            TOK_TYPEOF,
            TOK_NEW,
            TOK_IN,
            TOK_INSTANCEOF,
            TOK_DO,
            TOK_WHILE,
            TOK_FOR,
            TOK_BREAK,
            TOK_CONTINUE,
            TOK_SWITCH,
            TOK_CASE,
            TOK_DEFAULT,
            TOK_THROW,
            TOK_TRY,
            TOK_CATCH,
            TOK_FINALLY,
            TOK_FUNCTION,
            TOK_DEBUGGER,
            TOK_WITH,
            TOK_CLASS,
            TOK_CONST,
            TOK_ENUM,
            TOK_EXPORT,
            TOK_EXTENDS,
            TOK_IMPORT,
            TOK_SUPER,
            TOK_IMPLEMENTS,
            TOK_INTERFACE,
            TOK_LET,
            TOK_PACKAGE,
            TOK_PRIVATE,
            TOK_PROTECTED,
            TOK_PUBLIC,
            TOK_STATIC,
            TOK_YIELD,
        ];
        for (idx, tok) in tokens.iter().enumerate() {
            assert_eq!(*tok, TOK_FIRST_KEYWORD + idx as i32);
        }
    }

    #[test]
    fn regexp_allowed_matches_c_switch() {
        let disallow = [
            TOK_NUMBER,
            TOK_STRING,
            TOK_REGEXP,
            TOK_DEC,
            TOK_INC,
            TOK_NULL,
            TOK_FALSE,
            TOK_TRUE,
            TOK_THIS,
            TOK_IF,
            TOK_WHILE,
            TOK_FOR,
            TOK_DO,
            TOK_CASE,
            TOK_CATCH,
            TOK_IDENT,
            b')' as i32,
            b']' as i32,
        ];
        for tok in disallow {
            assert!(!is_regexp_allowed(tok));
        }
        let allow = [TOK_EOF, TOK_PLUS_ASSIGN, b'(' as i32, b'{' as i32];
        for tok in allow {
            assert!(is_regexp_allowed(tok));
        }
    }
}
