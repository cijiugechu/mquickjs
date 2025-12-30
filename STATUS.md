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
- Ported `mquickjs-c/softfp_template.h` arithmetic helpers into `src/softfp.rs` (sf64 add/sub/mul/div/sqrt, comparisons, sf32<->sf64 conversions) with tests.
- Ported `mquickjs-c/libm.{c,h}` into `src/js_libm.rs` (floor/ceil/trunc/round_inf/fabs/sqrt/lrint/scalbn, rem_pio2 + sin/cos/tan, asin/acos/atan/atan2, exp/log/log2/log10/pow) with tests and wired into `src/lib.rs`.
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
- Ported core JS string runtime helpers into `src/string/runtime.rs` (UTF-8/UTF-16 position conversion, length/getc, surrogate-aware substring splitting) and wired them into `src/context.rs` with tests.
- Ported atom/unique-string table management into `src/atom.rs` (sorted lookup, UTF-16 compare semantics, numeric string detection, GC-style sweep) with tests.
- Added full `JSProperty` layout and `JSClosureData` into `src/property.rs` and `src/closure_data.rs` with roundtrip tests.
- Implemented runtime object property tables in `src/property.rs` (hash lookup, create/define/update/delete, ROM promotion, array index fast path) with unit tests.
- Made ROM atom decoding Miri-safe and stabilized heap storage provenance in `src/context.rs` (`HeapStorage` + raw-bit ROM offsets) to avoid stacked-borrows UB.
- Reworked ROM table ownership + ROM property lookup to preserve pointer provenance under Miri and tightened stdlib Object/Function prototype-chain init (`src/context.rs`, `src/property.rs`).
- Added a read-only `JSContext` shell (`ContextShell`) capturing memory map fields and invariants in `src/context_shell.rs` with validation tests.
- Added read-only views for `JSFloat64`, `JSByteArray`, and `JSValueArray` in `src/memblock_views.rs` with roundtrip tests.
- Ported `JSObject` layout plus `JSRegExp`/`JSObjectUserData` into `src/object.rs` with header/payload tests.
- Ported `JSFunctionBytecode` layout into `src/function_bytecode.rs` with header/roundtrip tests.
- Ported heap/memblock allocator + free-list compaction infrastructure into `src/heap.rs` (HeapLayout, memblock sizing, threaded-pointer compaction) with tests, and added internal pointer accessors needed for GC threading.
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
- Added `JSParseState` shell plus `reset_parse_state` and buffer invariant tests in `src/parser/parse_state.rs`.
- Updated parser parse-state tests to run under Miri with real buffer pointers (no integer-to-pointer casts).
- Added parser parse stack helpers (`JS_STACK_SLACK`, push/pop, `parse_call` dispatcher) into `src/parser/parse_stack.rs` with unit tests for stack bottom updates and call/return sequencing.
- Added parser error helpers (`parse_expect*`, ASI handling) and `ParserError` in `src/parser/error.rs` with tests.
- Ported parser bytecode emission helpers (`emit_u*`, `emit_op*`, `emit_insert`, label patching, `emit_var`, short-int pushes) into `src/parser/emit.rs` with tests, plus pc2line state restore helpers in `src/parser/pc2line.rs`.
- Ported parser bytecode stack size analysis (`compute_stack_size`/`compute_stack_size_push`) into `src/parser/stack_size.rs` with tests for branches, npop calls, and error paths.
- Ported parser lvalue helpers (`get_lvalue`/`put_lvalue`) into `src/parser/lvalue.rs` with bytecode-level tests; exposed an emitter `pc2line_source_pos` accessor and enabled emit/lvalue tests under Miri.
- Ported parser control-flow helpers (`push_break_entry`/`pop_break_entry`, `emit_return`, `emit_break`) into `src/parser/control_flow.rs` with tests, and allowed `emit_goto` to patch `OP_gosub`.
- Ported parser variable/ext-var resolution (`find_*`, `add_*`, `define_var`, `put_var`, `convert_ext_vars_to_local_vars`, `resolve_var_refs`) into `src/parser/vars.rs` with owned array allocators and unit tests.
- Ported `js_parse_property_name` into `src/parser/property_name.rs` with get/set disambiguation, method detection, and tests.
- Ported expression parsing (`js_parse_postfix_expr`, `js_parse_unary`, `js_parse_expr_binary`, `js_parse_logical_and_or`, `js_parse_cond_expr`, `js_parse_assign_expr`, `js_parse_expr_comma`) into `src/parser/expr.rs` with opcode-level tests for precedence, array literals, assignments, and logical short-circuiting.
- Ported statement/block parsing (`js_parse_block`, `js_parse_statement`, `js_parse_var`, `js_skip_expr`, eval-ret handling) into `src/parser/expr.rs` with opcode-level tests for var inits, if/else, and return.
- Ported function/program parsing (`js_parse_function_decl`, `js_parse_function`, `js_parse_source_element`, `js_parse_program`, `js_parse_local_functions`) into `src/parser/expr.rs` with hoisted `OP_fclosure` emission, object getter/setter/method handling, and tests; added bytecode header mutators + pc2line hoisted-length helpers and enabled `function_bytecode` tests under Miri.
- Added parser entry dispatcher in `src/parser/entry.rs` with JS_Parse2-style eval-flag routing (program/JSON/regexp), unified error line/column mapping, and tests; exposed parser `has_column` toggles for `JS_EVAL_STRIP_COL`.
- **Parser runtime allocator (temporary, important):** Added `src/parser/runtime.rs` with a minimal heap-backed float64 allocator (minus-zero cache + short-float on 64-bit). `src/parser/expr.rs` now allocates numeric constants through this runtime instead of boxing `Float64View`. This is still **not** GC-managed or compacted; parser-owned constants must remain rooted when runtime/GC is introduced.
- Ported GC reference helpers (`JSGCRef` + JS_*GCRef stack/list operations) into `src/gc_ref.rs` (`push_gc_ref`, `pop_gc_ref`, `add_gc_ref`, `delete_gc_ref`) with tests, using `intrusive-collections` for intrusive lists. The GC ref slot now uses `UnsafeCell` + raw `JSValue*`-style access to match C semantics and stay Miri-safe while linked.
- Added GC mark phase + root enumeration in `src/gc.rs` (bounded mark stack, overflow rescans, object/value-array traversal) with tests; exposed GC root helpers for atom ROM tables, parse state, GC refs, and string pos cache cleanup.
- Added GC sweep/coalesce pass in `src/gc.rs` to reset marks, merge free blocks, and optionally call user-class finalizers, with tests covering finalizer calls and coalescing.
- Added GC runtime glue in `src/gc_runtime.rs` to run mark/sweep/compact over explicit root sets (class/stack refs, GC refs, atom tables, string pos cache) and refresh parse-state `source_buf` after compaction, with unit tests.
- Added Rust-only stdlib/bytecode definitions in `src/stdlib_def.rs` (builtin prototype enum + bytecode header constants/structs), avoiding C ABI function tables.
- Ported bytecode relocation helpers (`JS_IsBytecode`, `JS_RelocateBytecode2`, `JS_RelocateBytecode`) into `src/bytecode.rs` with strict-provenance-safe pointer rebasing, pointer-relocation tests, and a unique-string resolver hook.
- Ported 64-bit to 32-bit bytecode heap export (`gc_compact_heap_64to32`, short-float expansion, and prepare helpers) into `src/bytecode.rs` with conversion/overflow tests; added GC root builder helper in `src/gc_runtime.rs` and JSValue raw-bit accessors in `src/jsvalue.rs`.
- Wired stdlib metadata to builtin prototypes in `src/stdlib.rs` with helpers for typed iteration and a test ensuring all cproto names map to known variants.
- Expanded `build.rs` stdlib metadata to cover `mquickjs-c/mqjs_stdlib.c` (builtins/classes/typed arrays/error hierarchies/global props), keeping magic identifiers and duplicate entries aligned with C.
- Added stdlib cfunction dispatch scaffolding in `src/stdlib/cfunc.rs`, wiring cfunction tables into `JSContext` and interpreter short/cfunction call paths; mapped `f_f` to `src/js_libm.rs` and added magic/class-id mapping tests.
- Ported initial stdlib builtins for `Number`/`Math`/`Date.now`/`isNaN`/`isFinite` (constructors + formatting helpers + `Math` helpers) with tests, and aligned `dtoa` free/exponent + fixed rounding behavior with C (`JS_RNDNA`) for number formatting.
- Ported C API layout definitions from `mquickjs.h` into `src/capi_defs.rs` (JSCStringBuf, JSCFunctionDefKind/JSCFunctionType/JSCFunctionDef, JSSTDLibraryDef) with size/align/offset tests and an opaque `JSContext` handle type.
- Ported `JSWriteFunc` and `JSInterruptHandler` typedefs into `src/capi_defs.rs`.
- Ported `JS_EVAL_*` flags into `src/capi_defs.rs` with constant-value tests.
- Added writable runtime/context foundations in `src/context.rs` and `src/runtime.rs`, including JSContext initialization, ROM atom relocation, minimal object allocation, and tests covering heap layout and root initialization.
- Integrated runtime atoms/strings: ROM atom decode now allocates heap-backed strings, added `JSContext` string creation/interning helpers, and atom tables operate on heap string headers with new tests for empty/char strings and ROM atom placement.
- Ported `JS_NewFloat64` behavior into `JSContext::new_float64` (short-int fast path, minus-zero cache, short-float range) and wired parser numeric constant allocation through the runtime; removed the temporary `src/parser/runtime.rs` allocator.
- Added runtime helpers for byte arrays/function bytecode/closure objects in `src/context.rs`, updated `JSClosureData` to a flexible-array layout in `src/closure_data.rs`, and introduced a minimal interpreter loop in `src/interpreter.rs` (stack/frame setup, basic opcodes, Miri-safe stack restore) with tests.
- Extended `src/interpreter.rs` with control-flow and stack-shuffle opcodes (`OP_goto`/`OP_if_*`/`OP_gosub`/`OP_ret`/`OP_catch`/`OP_throw`, plus `OP_nip`/`OP_dup2`/`OP_insert2`/`OP_insert3`/`OP_perm3`/`OP_perm4`/`OP_rot3l`) and added execution tests for branching and exception flow.
- Added `src/conversion.rs` with `JS_ToPrimitive`/`JS_ToNumber`/`JS_ToPropertyKey` behavior, string concatenation helpers, and exported `string_compare`/`string_eq` from `src/atom.rs`.
- Extended `src/interpreter.rs` with arithmetic/bitwise/relational/equality/type operators (`OP_add/sub/mul/div/mod/pow`, `OP_inc/dec/post_*`, `OP_and/or/xor/shl/shr/sar/not`, `OP_lt/lte/gt/gte/eq/neq/strict_eq`, `OP_typeof`/`OP_in`/`OP_instanceof`) plus new opcode tests (string concat, equality, relational string compare, bitwise).
- Implemented object/array/property opcodes in `src/interpreter.rs` (`OP_get_field/get_field2`, `OP_put_field`, `OP_define_*`, `OP_get/put_array_el`, `OP_get_length/get_length2`, `OP_set_proto`, `OP_delete`) with interpreter tests and a test-only property inspector in `src/property.rs`.
- Implemented function/closure/varref opcodes in `src/interpreter.rs` (`OP_call*`, `OP_fclosure`, `OP_arguments`, `OP_this_func`, `OP_new_target`, `OP_get/put_var_ref*`, `OP_array_from`), added call-frame/varref handling plus interpreter tests; added `Context::alloc_array` and a raw property lookup helper for global varrefs.
- Implemented iteration + RegExp opcodes in `src/interpreter.rs` (`OP_for_in_start`, `OP_for_of_start`, `OP_for_of_next`, `OP_regexp`) and added `object_keys` for for-in enumeration (arrays + typed arrays) with interpreter tests.
- Ported the RegExp runtime executor into `src/regexp.rs` (bytecode interpreter, last_index updates, exec/test/search modes) with unit tests, and made stack restoration Miri-safe.
- Integrated GC collection into runtime allocations: `JSContext` now triggers mark/sweep/compact during heap pressure with stack/class roots, atom tables, string pos cache, GC refs, and registered parser state; parser registers/restores its `JSParseState` for GC safety.
- Routed parser strings/atoms through the runtime: lexer now interns/allocates via `JSContext`, property names are atomized, lexer tokens sync into `JSParseState` roots, and parser attach/detach runs after boxing for stable GC-visible pointers.
- Fixed Miri stacked-borrows in parser parse-state tracking by switching `JSParseState` fields to interior mutability (`Cell`), syncing lexer tokens via raw pointer writes to avoid aliasing, and using a Miri-only pointer exposure when attaching parse state so GC/lexer pointers survive interior updates.
- Added public API module in `src/api.rs` with `js_eval`, `js_parse`, `js_call`, `js_run` entry points; routes JSON parsing through `src/parser/json.rs`, RegExp compilation through `src/parser/regexp.rs`, and JavaScript program evaluation through the parser/interpreter pipeline with tests for JSON values and basic object/array creation.
- Extended `src/builtins.rs` with Object builtins (constructor, hasOwnProperty, toString, defineProperty, getPrototypeOf, setPrototypeOf, create, keys), Function builtins (constructor, call, apply, bind, toString, get/set prototype, get_length_name), String builtins (constructor, length, slice, substring, charAt, charCodeAt, codePointAt, fromCharCode, fromCodePoint, concat, indexOf, lastIndexOf, toLowerCase, toUpperCase, trim, trimStart, trimEnd, split, replace, replaceAll), Array builtins (constructor, length, push, pop, shift, unshift, join, toString, isArray, reverse, concat, indexOf, lastIndexOf, slice, splice, every, some, forEach, map, filter, reduce, reduceRight, sort), and JSON builtins (parse, stringify).
- Implemented `JSON.stringify` replacer/space handling (toJSON hook, replacer function/array, spacing/indentation, circular checks) and added integration tests for replacer/space/undefined output.
- Added full TypedArray/ArrayBuffer builtins in `src/builtins.rs` (constructors, length/byteLength/byteOffset/buffer getters, subarray, element get/set with per-type coercion) with unit tests.
- Created integration test framework in `tests/integration_tests.rs` with JSON parsing and global object tests; wired `src/stdlib/cfunc.rs` to dispatch all new builtins.
- Added regexp-aware String helpers (`match`, `search`, RegExp-aware `split`/`replace`) in `src/builtins.rs` and wired stdlib entries in `src/stdlib/cfunc.rs`; aligned regexp quantifier capture reset ordering with C and added a C Catch2 bytecode dump test for the tag regexp in `ctests/parser_bytecode_catch2_test.cpp`.
- Ported Boolean builtin constructor (`js_boolean_constructor`) with C-aligned truthiness semantics and stdlib dispatch wiring, plus unit tests.
- Ported RegExp builtins (`js_regexp_constructor`, `js_regexp_get_lastIndex`/`set_lastIndex`, `js_regexp_get_source`, `js_regexp_get_flags`, `js_regexp_exec`/test) into `src/builtins.rs` with stdlib dispatch wiring and unit tests.

## Known gaps (non-CLI/REPL)
- Date builtin partial: only `Date.now`; constructor/prototype methods are missing.
- Global `eval` builtin not wired (`js_global_eval`).
- Error/exception helpers are incomplete: `JS_Throw*` equivalents and backtrace/stack formatting are missing.
- Public C API functions are not ported yet (`JS_NewContext`, `JS_FreeContext`, `JS_Eval`, `JS_Parse`, `JS_Call`, etc.; only layouts/constants exist).

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

## Runtime roadmap (no CLI, stdlib required)

1. **Runtime/Context foundations:** implement writable `JSRuntime`/`JSContext` (new `src/runtime.rs`, `src/context.rs`) and wire them to `src/heap.rs`, `src/gc.rs`, and `src/gc_runtime.rs` for real allocations; normalize use of `src/jsvalue.rs`/`src/tagged_ptr.rs` in runtime paths.
2. **GC roots + invariants:** connect parse state, atom tables, string caches, and temporary roots to the GC root enumeration; ensure `src/memblock.rs`, `src/containers.rs`, and `src/memblock_views.rs` are used for allocation validation, not just layout tests.
3. **Atoms + strings in runtime:** bind `src/atom.rs`, `src/string/js_string.rs`, `src/string/string_char_buf.rs`, and `src/string/string_pos_cache.rs` to runtime lifetime and GC ownership (heap-backed strings + ROM atom decode wired; remaining string ops pending).
4. **Object + property system:** finish runtime behavior in `src/object.rs`, `src/property.rs`, and `src/rom_class.rs` (prototype chain, property lookup/define/update, class initialization hooks).
5. **Functions + bytecode objects:** make `src/function_bytecode.rs`, `src/bytecode.rs`, and `src/closure_data.rs` real GC-allocated objects; hook up cpool/ext_vars/varref storage, and wire native-call bridges from `src/cfunction_data.rs`.
6. **Interpreter loop:** port the opcode execution loop and call frames from `mquickjs-c/mquickjs.c` into a Rust interpreter module (e.g. `src/interpreter.rs`) that executes `src/opcode.rs`.
7. **Parser integration:** replace the temporary allocator in `src/parser/runtime.rs` with GC-managed allocations; route `src/parser/entry.rs` through `JS_Parse2`/`JS_Parse`/`JS_Eval` and map `ParserError` to runtime exceptions.
8. **Stdlib (no CLI):** port builtin initialization from `mquickjs-c/mqjs_stdlib.c` + `mquickjs-c/mquickjs.c`, registering globals via `src/stdlib/mod.rs`, `src/stdlib/stdlib_def.rs`, and `src/stdlib/stdlib_image.rs`.
9. **Runtime tests:** drive eval-based tests from `mquickjs-c/tests/*.js` without CLI, validating parser/runtime/stdlib integration.

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


