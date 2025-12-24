# Rust port of the C opus library

The directory where the C version is located is `/mquickjs-c`.

# Porting strategy

- Before porting functions and structures, first find out what their invariant is.

- Always port the implementation and tests together.
- If a C feature has no meaningful tests to port, do not add placeholder roundtrip tests just to have tests.

- When using constants for mathematical operations, give priority to whether rust core provides them.

- Do not port trivial one-line wrappers or renamings that add no semantics; use Rust std/core APIs directly to keep the port lean.
- For JSString storage in Rust, do not rely on implicit NUL-termination. Store raw bytes and use explicit length; if a C API needs NUL-terminated data, introduce a local/temporary buffer or helper instead of embedding trailing NULs in JSString.

- Run `cargo check --all-features`, `cargo test --all-features` and `cargo clippy` automatically after making code changes, you need to make sure no errors.


## Best practices for handling implicit invariants when rewriting C libraries in Rust

### 1. Discovery Phase: Expose Hidden Assumptions
*   **Dynamic Analysis:** Run the original C library with **Sanitizers** (ASan, UBSan, TSan) to reveal runtime invariants and undefined behaviors disguised as valid logic.
*   **Invariant Categorization:** Explicitly map out:
    *   **Lifetimes:** Data pointer validity vs. struct lifespan.
    *   **Ownership:** Who `free`s the memory? (Caller vs. Callee).
    *   **Thread Safety:** Reentrancy and global state locks.
    *   **Data Constraints:** Valid ranges for primitive types (e.g., `int` used as enum or ID).

### 2. Encoding Phase: Shift Runtime to Compile-Time
Leverage the Rust Type System to enforce semantics.
*   **Type State Pattern:** Encode State Machines (e.g., `Init` $\to$ `Configured` $\to$ `Running`) to enforce call order at compile time.
*   **Newtype Pattern:** Wrap primitives (e.g., `struct FileId(u32)`) to prevent logic errors like swapping arguments.
*   **RAII:** Implement the `Drop` trait to handle resource cleanup automatically, replacing complex C `goto fail` patterns.
*   **Option/Result:** Replace `NULL` and error codes (e.g., `-1`) with `Option<NonNull<T>>` and `Result<T, E>`.
*   **Concurrency:** Manually implement `Send` and `Sync` only if the underlying C logic is verifiably thread-safe.

### 3. Boundary Handling: Managing `unsafe`
*   **The Unsafe Sandwich:** Encapsulate minimal `unsafe` FFI calls within a robust Safe Rust API.
*   **Defensive Assertions:** Use `assert!` before `unsafe` blocks to validate C-inputs (e.g., index bounds) at runtime.
*   **Safety Comments:** Mandate `// SAFETY:` comments explaining *why* a specific `unsafe` block does not trigger UB.
*   **Panic Safety:** Use `std::panic::catch_unwind` at FFI boundaries to prevent Rust panics from crashing the C runtime (UB).

### 4. Verification Strategy
*   **Differential Fuzzing:** Use `libfuzzer`/`cargo-fuzz` to feed identical inputs to both C and Rust versions and compare outputs.
*   **Miri Execution:** Run tests via `cargo miri test` to detect Undefined Behavior (alignment, stacked borrows) that standard compilers miss.
*   **Property-Based Testing:** Use `proptest` to verify invariants hold across generated edge cases.

### 5. Implementation Strategy
*   **Incremental Rewrite:** Replace "leaves" (dependency-free modules) first.
*   **Internal Replacement:** Keep C-compatible ABI/API initially; link Rust code as a static library to the existing C project.

## Example: C port -> Rust idiomatic

Use the `memblock` + `containers` refactor as a template for migrating C-style bitfields into Rust types:

- Introduce `MTag` as a `repr(u8)` enum instead of raw `JSWord` tag constants, with `TryFrom<JSWord>` for validated conversions.
- Use newtypes (`MbHeader`, `StringHeader`, `ByteArrayHeader`, `ValueArrayHeader`, `VarRefHeader`) to encode header layouts and enforce invariants.
- Move header construction and decoding into methods on the types, avoiding free functions that pass raw `JSWord` around.
- Keep bit-level layout explicit (`word()` access) for low-level packing while presenting safe, typed APIs to callers.
- Update tests alongside the refactor to validate tag extraction and size round-trips.

Reference implementation: `src/memblock.rs` and `src/containers.rs`.

## Rust src tree

```
src/
  array_buffer.rs
  array_data.rs
  capi_defs.rs
  cfunction_data.rs
  closure_data.rs
  containers.rs
  context_shell.rs
  cutils.rs
  dtoa.rs
  enums.rs
  error_data.rs
  function_bytecode.rs
  gc_ref.rs
  js_libm.rs
  jsvalue.rs
  lib.rs
  list.rs
  memblock.rs
  memblock_views.rs
  object.rs
  opcode.rs
  parser/
    lexer.rs
    mod.rs
    pos.rs
    regexp_flags.rs
    tokens.rs
    types.rs
  property.rs
  rom_class.rs
  softfp.rs
  stdlib/
    mod.rs
    stdlib_def.rs
    stdlib_image.rs
  string/
    mod.rs
    string_char_buf.rs
    string_pos_cache.rs
  tagged_ptr.rs
  typed_array.rs
```
