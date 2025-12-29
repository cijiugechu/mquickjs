# Roadmap: Interpreter and Runtime Parity

## Current gaps

### Interpreter coverage
- C dispatches 116 opcodes; Rust interpreter still missing some groups (see roadmap items).
- Previously missing loop/regexp opcodes (`OP_for_in_start`, `OP_for_of_start`, `OP_for_of_next`, `OP_regexp`) are now implemented.
- Discrepancy: `OP_push_const8` is handled in Rust but does not appear in C dispatch.

### Runtime and builtins
- Builtin gap: C declares 84 `js_*` builtins, Rust implements 18 (69 missing).
- Missing families: Object, Function, String, Array, JSON, TypedArray, ArrayBuffer, Error, Date, Boolean. RegExp builtins are still missing; runtime exec is now in place.
- Core conversions and slow paths exist in Rust (conversion module + interpreter slow paths); `JS_ToObject` still lacks primitive boxing.
- Exceptions and Error objects are incomplete; `JS_Throw*` and error formatting are not implemented.
- Public C API is largely absent: `JS_NewContext`, `JS_FreeContext`, `JS_Eval`, `JS_Parse`, `JS_Call`, etc. Only layouts and constants exist.

## Port roadmap

1. **Control flow + stack ops first** (done)
   - Implemented `OP_goto`, `OP_if_true`, `OP_if_false`, `OP_ret`, `OP_gosub`, `OP_catch`, `OP_throw`.
   - Added stack shuffles: `OP_nip`, `OP_dup2`, `OP_insert2`, `OP_insert3`, `OP_perm3`, `OP_perm4`, `OP_rot3l`.
   - Added execution tests for branches and exception flow.
2. **Core conversions and slow paths** (done)
   - Implemented `JS_ToPrimitive`, `JS_ToNumber`, `JS_ToPropertyKey`, plus string compare/eq helpers.
   - Ported `js_add_slow`/arith/logic/relational/equality slow-path behavior and wired the interpreter to use them.
   - `JS_ToObject` currently only validates object inputs; primitive boxing remains.
3. **Object, array, and property opcodes** (done)
   - Implemented `OP_get_field/get_field2`, `OP_put_field`, `OP_define_*`, `OP_get/put_array_el`, `OP_get_length/get_length2`, `OP_set_proto`, `OP_delete`.
   - Wired to `src/property.rs` and `src/object.rs` semantics with interpreter tests.
4. **Functions, closures, and varrefs** (done)
   - Implemented `OP_fclosure`, `OP_call*`, `OP_arguments`, `OP_this_func`, `OP_new_target`, `OP_get/put_var_ref*`, `OP_array_from`.
   - Added interpreter tests for calls, constructor/new_target, arguments, array_from, and varref behavior.
5. **Iteration and RegExp** (done)
   - Implemented `OP_for_in_start`, `OP_for_of_start`, `OP_for_of_next`.
   - Implemented `OP_regexp` and connected RegExp exec runtime with tests.
6. **Builtins and public API** (in progress)
   - Port missing builtin families in priority order: Object/Function/String/Array, then Error/RegExp/JSON, then TypedArray/ArrayBuffer.
   - [x] Added public API module (`src/api.rs`) with `js_eval`, `js_parse`, `js_call`, `js_run` entry points.
   - [x] API routes JSON evaluation, RegExp compilation, and JavaScript program execution through the parser and interpreter.
   - [ ] Start running `mquickjs-c/tests/*.js` as integration tests once bytecode evaluation is stable.

## References
- `mquickjs-c/mquickjs.c` (opcode dispatch and runtime logic)
- `src/interpreter.rs` (current Rust interpreter)
- `mquickjs-c/mquickjs_priv.h` and `src/builtins.rs` (builtin surface)
- `mquickjs-c/mquickjs.h` and `src/capi_defs.rs` (public API surface)
