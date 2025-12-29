# Integration Tests Ignore Reasons and Fix Options

## Why the TypedArray/ArrayBuffer JS eval tests are ignored

1) stdlib is not initialized in the test context
- `tests/integration_tests.rs` uses `prepare_compilation: true`.
- In `src/context.rs`, `prepare_compilation` skips `init_stdlib`, so the
  global object does not get `ArrayBuffer`, `Uint8Array`, etc.
- Result: `js_eval` hits a ReferenceError when evaluating `new Uint8Array(...)`.

2) Getters for byteLength/length are not supported yet
- In C stdlib, TypedArray and ArrayBuffer expose accessors:
  - `TypedArray.length/byteLength/byteOffset/buffer` are getters.
  - `ArrayBuffer.byteLength` is a getter.
- In Rust `src/property.rs`, `get_property` returns an error for GetSet
  properties and does not call the getter.
- `get_length_value` only special-cases Array/String, so TypedArray `length`
  falls back to property access and fails.

3) The "new operator not implemented" note is out of date
- Parser emits `OP_CALL_CONSTRUCTOR` and interpreter handles it.
- The remaining blockers are stdlib init and missing GetSet handling.

## Fix options

Option A (minimal, keep scope small):
- Change the test context to `prepare_compilation: false` so stdlib is loaded.
- Add fast paths for TypedArray length/byteLength/byteOffset/buffer and
  ArrayBuffer.byteLength in property access or `get_length_value`.
- Keep GetSet unimplemented for now.

Option B (more correct, larger change):
- Implement GetSet property access:
  - `get_property` should call the getter function.
  - `set_property` should call the setter function.
  - This requires `get_property` to take `&mut JSContext`, and updating
    call sites.

## Recommendation

Start with Option A to unignore the tests quickly, then move to Option B for
general correctness of accessor properties.

## Investigation Update (after Option B + stdlib init tweaks)

1) Stdlib init alone does not fix the integration tests
- `tests/integration_tests.rs` now uses `prepare_compilation: false`, so stdlib is loaded.
- The TypedArray/ArrayBuffer JS eval tests still fail with `JS_EXCEPTION`.

2) The failure is a `ReferenceError` from unresolved globals during `js_eval`
- `js_eval(b"Uint8Array", 0)` throws and the exception string is `"undefined"`.
- The global object *does* contain `Uint8Array` as a `VarRef` property, so the binding exists.
- The parsed bytecode for `Uint8Array` shows `ext_vars_len=1`, meaning it relies on external var refs.

3) Root cause: `js_eval` builds a closure without resolving external vars
- In `src/api.rs`, `js_eval_internal` previously created a closure via `ctx.alloc_closure(...)`
  with `ext_vars_len=0`, leaving external/global references unresolved.
- `create_closure` in `src/interpreter.rs` is the correct path; it resolves ext vars
  (including globals) into var refs.
- Because globals weren’t wired, `Uint8Array` resolved to an exception at runtime,
  even with stdlib initialized.

4) Implication for Option B
- Implementing GetSet accessors was necessary for `length`/`byteLength`, but not sufficient.
- `js_eval` must use `create_closure` (or equivalent) so global references are resolved.

## Next Fix (candidate)

Option B+ (required follow-up):
- In `src/api.rs`, when handling `ParsedResult::Bytecode`, create the closure via
  `create_closure(ctx, func_bytecode, None)` instead of `ctx.alloc_closure(...)`.
- This should resolve global references like `Uint8Array`/`ArrayBuffer` during eval
  and unblock the integration tests.
