# Rust port of the C opus library

The directory where the C version is located is `/mquickjs-c`.

# Porting strategy

- Before porting functions and structures, first find out what their invariant is.

- Always port the implementation and tests together.

- When using constants for mathematical operations, give priority to whether rust core provides them.

- Do not port trivial one-line wrappers or renamings that add no semantics; use Rust std/core APIs directly to keep the port lean.

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
