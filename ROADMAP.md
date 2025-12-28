# Roadmap: Interpreter and Runtime Parity

## Current gaps

### Interpreter coverage
- C dispatches 116 opcodes, Rust interpreter handles 53, leaving 64 missing.
- Notable missing groups:
  - Control flow and exceptions: `OP_goto`, `OP_if_true`, `OP_if_false`, `OP_ret`, `OP_gosub`, `OP_catch`, `OP_throw`.
  - Calls, closures, varrefs: `OP_call`, `OP_call_constructor`, `OP_call_method`, `OP_fclosure`, `OP_arguments`, `OP_this_func`, `OP_new_target`, `OP_get_var_ref`, `OP_put_var_ref`, `OP_get_var_ref_nocheck`, `OP_put_var_ref_nocheck`, `OP_array_from`.
  - Object/array/property: `OP_object`, `OP_get_field`, `OP_get_field2`, `OP_put_field`, `OP_define_field`, `OP_define_getter`, `OP_define_setter`, `OP_get_array_el`, `OP_get_array_el2`, `OP_put_array_el`, `OP_get_length`, `OP_get_length2`, `OP_set_proto`, `OP_delete`.
  - Operators and comparisons: `OP_and`, `OP_or`, `OP_xor`, `OP_shl`, `OP_shr`, `OP_sar`, `OP_pow`, `OP_not`, `OP_typeof`, `OP_in`, `OP_instanceof`, `OP_lt`, `OP_lte`, `OP_gt`, `OP_gte`, `OP_inc`, `OP_dec`, `OP_post_inc`, `OP_post_dec`.
  - Stack ops, loops, regexp: `OP_nip`, `OP_dup2`, `OP_insert2`, `OP_insert3`, `OP_perm3`, `OP_perm4`, `OP_rot3l`, `OP_for_in_start`, `OP_for_of_start`, `OP_for_of_next`, `OP_regexp`.
- Discrepancy: `OP_push_const8` is handled in Rust but does not appear in C dispatch.

### Runtime and builtins
- Builtin gap: C declares 84 `js_*` builtins, Rust implements 18 (69 missing).
- Missing families: Object, Function, String, Array, RegExp, JSON, TypedArray, ArrayBuffer, Error, Date, Boolean.
- Core conversions and slow paths are missing: `JS_ToPrimitive`, `JS_ToNumber`, `JS_ToPropertyKey`, `JS_ToObject`, plus `js_add_slow` and `js_binary_op` equivalents.
- Exceptions and Error objects are incomplete; `JS_Throw*` and error formatting are not implemented.
- Public C API is largely absent: `JS_NewContext`, `JS_FreeContext`, `JS_Eval`, `JS_Parse`, `JS_Call`, etc. Only layouts and constants exist.

## Port roadmap

1. **Control flow + stack ops first**
   - Implement `OP_goto`, `OP_if_true`, `OP_if_false`, `OP_ret`, `OP_gosub`, `OP_catch`, `OP_throw`.
   - Add missing stack shuffles: `OP_nip`, `OP_dup2`, `OP_insert2`, `OP_insert3`, `OP_perm3`, `OP_perm4`, `OP_rot3l`.
   - Add execution tests for branches and exception flow.
2. **Core conversions and slow paths**
   - Implement `JS_ToPrimitive`, `JS_ToNumber`, `JS_ToPropertyKey`, `JS_ToObject`.
   - Port `js_add_slow`/`js_binary_op` behavior for mixed-type arithmetic and comparisons.
   - Extend interpreter to use slow paths where required.
3. **Object, array, and property opcodes**
   - Implement `OP_get_field/get_field2`, `OP_put_field`, `OP_define_*`, `OP_get/put_array_el`, `OP_get_length`, `OP_set_proto`, `OP_delete`.
   - Wire to `src/property.rs` and `src/object.rs` for prototype and property semantics.
4. **Functions, closures, and varrefs**
   - Implement `OP_fclosure`, `OP_call*`, `OP_arguments`, `OP_this_func`, `OP_new_target`, `OP_get/put_var_ref*`.
   - Ensure GC roots include call frames, arguments, and varrefs.
5. **Iteration and RegExp**
   - Implement `OP_for_in_start`, `OP_for_of_start`, `OP_for_of_next`.
   - Implement `OP_regexp` and connect to RegExp exec runtime.
6. **Builtins and public API**
   - Port missing builtin families in priority order: Object/Function/String/Array, then Error/RegExp/JSON, then TypedArray/ArrayBuffer.
   - Implement public API entry points (`JS_NewContext`, `JS_Eval`, `JS_Parse`, `JS_Call`) and connect to parser/runtime/stdlib.
   - Start running `mquickjs-c/tests/*.js` as integration tests once evaluation works.

## References
- `mquickjs-c/mquickjs.c` (opcode dispatch and runtime logic)
- `src/interpreter.rs` (current Rust interpreter)
- `mquickjs-c/mquickjs_priv.h` and `src/builtins.rs` (builtin surface)
- `mquickjs-c/mquickjs.h` and `src/capi_defs.rs` (public API surface)
