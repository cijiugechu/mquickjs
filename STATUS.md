# Status

## MQuickJS-C leaf modules

Based on local `#include "..."` dependencies (considering both `.c` and `.h`), the leaf modules are:

- `mquickjs-c/cutils.c` + `mquickjs-c/cutils.h`
- `mquickjs-c/list.h`
- `mquickjs-c/mquickjs_opcode.h`
- `mquickjs-c/softfp_template_icvt.h`

## Porting progress

- Added `intrusive-collections = "0.9.7"` to model the `list.h` intrusive list pattern.
- Added `zerocopy = "0.8.31"` and ported `mquickjs-c/cutils.{h,c}` into `src/cutils.rs` (UTF-8 helpers, unaligned read/write helpers, bit ops, and small string/hex utilities) with tests.
- Ported `mquickjs-c/list.h` into `src/list.rs` with tests covering insertion, iteration, and removal.
- Ported `mquickjs-c/mquickjs_opcode.h` into `src/opcode.rs` with metadata tables and ordering invariant tests.
- Confirmed the `#if 0` opcodes in `mquickjs-c/mquickjs_opcode.h` are unused in both build and runtime code; only a debug-printing branch references them and is also disabled, so Rust should ignore them unless a future feature flag is desired.
- Dropped trivial one-line wrappers and C-style `BOOL` aliases from `src/cutils.rs` in favor of direct Rust std/core usage, per the updated porting rule in `AGENTS.md`.
- Ported `mquickjs-c/softfp_template_icvt.h` into `src/softfp.rs` (sf32/sf64 integer<->float conversions and rounding modes) with tests.
- Ported a first libm slice from `mquickjs-c/libm.{c,h}` into `src/js_libm.rs` (floor/ceil/trunc/round_inf/fabs/sqrt/lrint/scalbn) with tests and wired into `src/lib.rs`.
- Ported `js_fmod` using the `fmod_sf64` soft-float implementation in `src/softfp.rs`, with edge-case tests (NaN/Inf/signed zero).
- Added a Rust `build.rs` + `crates/mquickjs-build` generator scaffold that mirrors `mquickjs_build.c` logic (atoms/properties/cfunc metadata) and emits a `stdlib_image.rs` table with tests around core invariants.
- Ported `mquickjs-c/dtoa.{h,c}` into `src/dtoa.rs` using `ryu-js`, `lexical-core`, and `libm` for JS-style number formatting/parsing (radix handling, minus-zero, exponent modes, legacy octal, underscores) with tests; flag handling now uses `bitflags`.
- Added `src/jsvalue.rs` for JSValue tagging helpers and tests, then refactored to strict-provenance-friendly tagged pointers via `src/tagged_ptr.rs` (using `map_addr/with_addr`), keeping short-float/int/special tag invariants.
- Updated tests to skip non-unsafe modules under Miri (`#[cfg(all(test, not(miri)))]`) and run strict-provenance Miri on the JSValue tests only.
- Ported JS memblock header & mtags layout into `src/memblock.rs`, including header encode/decode helpers and value array header size extraction tests.
- Ported JSString/JSByteArray/JSValueArray/JSVarRef header layouts and size helpers into `src/containers.rs`, with flag/length roundtrip tests.
- Added Rust-idiomatic newtypes for `JSFreeBlock`/`JSFloat64` headers in `src/memblock.rs` with roundtrip tag/size tests.
- Ported `JSStringPosCacheEntry` and related constants/enums into `src/string_pos_cache.rs` with tests.
- Added minimal enum ports for `JSPropTypeEnum` and `JSVarRefKindEnum` in `src/enums.rs` with discriminant tests.
- Added `JSObjectClassEnum` and `JSCFunctionEnum` ports in `src/enums.rs` with discriminant tests.
- Ported `JSProperty` bitfield metadata into `src/property.rs` with roundtrip tests.
- Ported `JSArrayData` layout and length invariant into `src/array_data.rs` with tests.
- Ported `JSErrorData` and `JSCFunctionData` into `src/error_data.rs` and `src/cfunction_data.rs` with roundtrip tests.
- Ported `JSTypedArray` into `src/typed_array.rs` with roundtrip tests.
- Ported `JSArrayBuffer` into `src/array_buffer.rs` with roundtrip tests.
- Ported `JSROMClass` and `JSStringCharBuf` into `src/rom_class.rs` and `src/string_char_buf.rs` with tests.
- Added `JSString` backing storage in `src/string/js_string.rs` (raw bytes + header, no implicit NUL) and wired it into `src/string/mod.rs`.
- Added full `JSProperty` layout and `JSClosureData` into `src/property.rs` and `src/closure_data.rs` with roundtrip tests.
- Added a read-only `JSContext` shell (`ContextShell`) capturing memory map fields and invariants in `src/context_shell.rs` with validation tests.
- Added read-only views for `JSFloat64`, `JSByteArray`, and `JSValueArray` in `src/memblock_views.rs` with roundtrip tests.
- Ported `JSObject` layout plus `JSRegExp`/`JSObjectUserData` into `src/object.rs` with header/payload tests.
- Ported `JSFunctionBytecode` layout into `src/function_bytecode.rs` with header/roundtrip tests.
- Ported parser data structures (`BlockEnv`, `JSSourcePos`, `JSToken`, `JSParsePos`) into `src/parser_types.rs`.
- Ported parser line/column helpers (`get_line_col`, `get_line_col_delta`) into `src/parser_pos.rs` with UTF-8 lead-byte counting tests.
- Ported pc2line bitstream decoding helpers (`get_bits`, `get_ugolomb`, `get_sgolomb`, `get_pc2line_hoisted_code_len`, `find_line_col`) into `src/parser/pc2line.rs` with roundtrip/tests for line/column decoding.
- Ported pc2line encoding/emit helpers (`pc2line_put_bits*`, `put_ugolomb`/`put_sgolomb`, `emit_pc2line`) as `Pc2LineEmitter` in `src/parser/pc2line.rs` with encoder/decoder roundtrip tests.
- Ported parser token/keyword constants plus `is_regexp_allowed` into `src/parser/tokens.rs` with ordering/mapping tests.
- Ported parser escape parsing + identifier ASCII helpers (`js_parse_escape`, `is_ident_first`, `is_ident_next`) into `src/parser/lexer.rs` with tests.
- Ported core lexer flow (`next_token`, `js_parse_string`, `js_parse_ident`, `js_parse_regexp_token`) plus a `ParseState` harness and sorted interned string table into `src/parser/lexer.rs` with tests.
- Ported parser position capture/restore (`js_parse_get_pos`, `js_parse_seek_token`) and paren skipping (`js_skip_parens`, `js_parse_skip_parens_token`) into `src/parser/lexer.rs` with tests.
- Ported regexp flag parsing (`js_parse_regexp_flags`) plus `LRE_FLAG_*` constants into `src/parser/regexp_flags.rs` with tests.
- Ported regexp bytecode parsing/emission (`re_parse_*`, `js_parse_regexp`) into `src/parser/regexp.rs` with non-recursive parse stack and compile-time tests for prefix/captures/errors.
- Ported JSON parsing (`js_parse_json_value`/`js_parse_json`) into `src/parser/json.rs` using a non-recursive stack and QuickJS string/number parsing semantics, with tests for basic objects, nesting depth, escapes, and error positions.
- Added parser parse-state constants/enums (`PARSE_STATE_*`, `PF_*`, parse function/property enums) into `src/parser/parse_state.rs` with discriminant tests.
- Added parser parse stack helpers (`JS_STACK_SLACK`, push/pop, `parse_call` dispatcher) into `src/parser/parse_stack.rs` with unit tests for stack bottom updates and call/return sequencing.
- Added parser error helpers (`parse_expect*`, ASI handling) and `ParserError` in `src/parser/error.rs` with tests.
- Ported GC reference helpers (`JSGCRef` + JS_*GCRef list operations) into `src/gc_ref.rs`, using `intrusive-collections` for intrusive lists.
- Added Rust-only stdlib/bytecode definitions in `src/stdlib_def.rs` (builtin prototype enum + bytecode header constants/structs), avoiding C ABI function tables.
- Wired stdlib metadata to builtin prototypes in `src/stdlib.rs` with helpers for typed iteration and a test ensuring all cproto names map to known variants.
- Ported C API layout definitions from `mquickjs.h` into `src/capi_defs.rs` (JSCStringBuf, JSCFunctionDefKind/JSCFunctionType/JSCFunctionDef, JSSTDLibraryDef) with size/align/offset tests and an opaque `JSContext` handle type.
- Ported `JSWriteFunc` and `JSInterruptHandler` typedefs into `src/capi_defs.rs`.
- Ported `JS_EVAL_*` flags into `src/capi_defs.rs` with constant-value tests.

## Parser port scope (C -> Rust)

- Primary sources: `mquickjs-c/mquickjs.c` (lexer/parser/bytecode emission), `mquickjs-c/mquickjs.h` (`JS_EVAL_*`, `JS_Parse`/`JS_Eval`), `mquickjs-c/mquickjs_priv.h` (parser debug toggles), `mquickjs-c/mquickjs_opcode.h` (OP/REOP tables).
- Core types/enums/macros: `JSParseState`, `JSToken`, `JSParsePos`, `BlockEnv`, `JSSourcePos`, `TOK_*`, `PF_*`, `ParseExprFuncEnum`, `JSParseFunctionEnum`, `PARSE_PROP_*`, `LABEL_*`, `SP_TO_VALUE`/`VALUE_TO_SP`, `JS_STACK_SLACK`.
- Lexer/positioning: `next_token`, `js_parse_escape`, `js_parse_string`, `js_parse_ident`, `js_parse_regexp_token`, `is_regexp_allowed`, `js_parse_get_pos`/`js_parse_seek_token`/`js_parse_skip_parens_token`, `skip_spaces`, `get_line_col`/`get_line_col_delta`.
- Parse engine (non-recursive): `parse_stack_alloc`, `js_parse_push_val`, `js_parse_pop_val`, `PARSE_START*` + `PARSE_CALL*`, `js_parse_call`, `parse_func_table`, `js_parse_*` expression/statement/block functions.
- Bytecode emission + control flow: `emit_*`/`emit_op_*`, `pc2line_put_bits*`, `put_ugolomb`/`put_sgolomb`, `emit_pc2line`, `new_label`/`emit_label`/`emit_goto`, `get_lvalue`/`put_lvalue`, `emit_return`/`emit_break`.
- Scope/variables: `define_var`/`put_var`, `add_var`/`find_var`, `add_ext_var`/`find_ext_var`/`add_func_ext_var`, `resolve_var_refs`, `compute_stack_size`, `convert_ext_vars_to_local_vars`.
- JSON/RegExp parse: `js_parse_json_value`/`js_parse_json`/`js_json_parse`, `js_parse_regexp`/`js_parse_regexp_flags`/`js_compile_regexp`, `re_parse_*` helpers, `REOP_*`/`LRE_FLAG_*` constants.
- Entry/error flow: `JS_Parse2`/`JS_Parse`/`JS_Eval`, `js_parse_error`/`_mem`/`_stack_overflow`, `js_parse_expect`/`js_parse_expect_semi` (setjmp/longjmp error path).
- Tests to port together: `test_json`, `test_large_eval_parse_stack`, `test_regexp`, `test_line_column_numbers` plus helpers `get_string_pos`, `check_error_pos`, `eval_error`.
- Key invariants to preserve: non-recursive parser + explicit parse stack, `TOK_*` ordering tied to `JS_ATOM_*`, `got_lf` + `is_regexp_allowed` for ASI/regexp literal disambiguation, parse stack uses JSValue + `JS_STACK_SLACK`, error positions (JSON/RegExp use `buf_pos`, JS parser uses `token.source_pos`), `ctx->parse_state` must track GC moves of `source_buf`.

## Cutils assessment

Port (custom Rust code required):
- UTF-8 helpers: `__unicode_to_utf8`, `__unicode_from_utf8`, `__utf8_get`, plus wrappers `unicode_to_utf8`, `unicode_from_utf8`, `utf8_get`. These accept surrogates and have specific error semantics not covered by `std::char`/`std::str`.
- Unaligned native-endian loads/stores: `get_u{8,16,32,64}`, `get_i{8,16,32,64}`, `put_u{8,16,32,64}`, plus `get_be32`/`put_be32` (bytecode parsing/serialization).
- Bit operations: `clz32/clz64/ctz32/ctz64` as wrappers over `leading_zeros`/`trailing_zeros` with the non-zero invariant enforced.
- `from_hex` (small helper, can be kept local).

Replace with Rust std/core:
- Min/max helpers (`max_int`, `min_int`, etc.) -> `Ord::max/min` or `std::cmp`.
- Byte swaps (`bswap16/32/64`) -> `swap_bytes`.
- Float/int bit casts (`float64_as_uint64`, `uint64_as_float64`, `float_as_uint`, `uint_as_float`) -> `to_bits`/`from_bits`.
- `pstrcpy`, `pstrcat`, `strstart`, `has_suffix` -> `String`/`str` helpers (`strip_prefix`, `ends_with`), or local equivalents if fixed buffers remain.

Skip/avoid direct port:
- C-only macros/attributes (`likely`, `unlikely`, `force_inline`, `no_inline`, `__maybe_unused`).
- `offsetof`, `countof`, `container_of` (use Rust layouts, `memoffset` if needed, or `intrusive-collections` adapters).

## Upcoming mquickjs.c/h port plan (incremental, correctness-first)

- **JSValue tagging & helpers**: Recreate tag layout (`JS_TAG_*`, special bits) and `JS_VALUE_GET/MAKE_*`/`JS_Is*` helpers in Rust newtypes with exhaustive tests (cover short-float on/off, int/pointer tagging).
- **Memblock header & mtags**: Model `JS_MTAG_BITS`/`JS_MB_HEADER` and mtags in Rust; add bit-width/packing tests for 32/64-bit layouts. Keep allocator in C initially; Rust only parses/validates headers.
- **Core containers**: Port layouts/constants for `JSString`/`JSByteArray`/`JSValueArray`/`JSVarRef` (length limits, flag bits) with property/roundtrip tests. Provide safe read views, no allocator takeover yet.
- **JSContext shell**: Define Rust view/FFI handle mirroring `JSContext` memory map invariants (sp > stack_bottom, min_free_size). Expose read-only queries; assert before any unsafe entry.
- **Objects & classes**: Model `JSObject` shape (class_id, extra_size, proto, props, union payloads) with typed accessors. Validate size/alignment math via tests; keep creation backed by C for now.
- **Bytecode metadata**: Use existing opcode port to parse `JSFunctionBytecode` fields (arg_count, stack_size, cpool/ext_vars/pc2line). Test with C-generated sample bytecode for field equality.
- **Interpreter bridge**: Keep C interpreter as oracle; add Rust wrappers for `JS_Call`/`JS_Eval` to collect outputs/exceptions for diff tests. Defer opcode execution port until data-model steps above are stable.
- **Builtins surface**: Use Rust stdlib metadata (`stdlib_image` + `BuiltinProto`) to register/dispatch builtins without C tables. Gradually port builtin families (Function/String/Array/Math/TypedArray) with proptest + differential tests.
- **Validation regimen**: Each increment runs `cargo check --all-features`, `cargo test --all-features`, `cargo clippy`; unsafe-heavy paths also run `cargo miri test`. Differential tests compare C/Rust outputs (incl. NaN/-0, UTF-16 edges, hash collisions).
