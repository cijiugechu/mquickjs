# CLI port plan (mqjs -> mquickjs-cli)

## C reference points
- `mquickjs-c/mqjs.c`: option parsing, eval flow, timers, REPL glue, CLI builtins.
- `mquickjs-c/readline.c` + `mquickjs-c/readline.h`: line editing, history, colors.
- `mquickjs-c/readline_tty.c` + `mquickjs-c/readline_tty.h`: raw TTY mode, Ctrl-C handling.
- `mquickjs-c/mqjs_stdlib.c`: stdlib image + builtin registrations used by the CLI context.

## Behavior invariants to preserve
- Default memory size is `16 << 20` bytes; `--memory-limit` parses `g/m/k` suffixes (base-1024).
- Option precedence: `-o` compiles bytecode and exits; `-e` evaluates expression; if no file, REPL starts.
- `--no-column` sets parse flag to strip column info.
- `-d` increments dump level; `>= 2` enables verbose dump in `JS_DumpMemory`.
- `-m32` only valid with `-o`, forces 32-bit bytecode output.
- `scriptArgs` is injected when a file is executed with remaining argv.
- `js_print` prints space-separated args and a trailing newline; strings are written raw bytes.
- `setTimeout`/`clearTimeout` store a bounded timer list and run after each eval or REPL input.
- REPL uses line-editing + history + syntax coloring; Ctrl-C requires a double-press to exit.

## Rust-side mapping (current codebase)
- Context creation: `JSRuntime::new(ContextConfig { image: &MQUICKJS_STDLIB_IMAGE, memory_size, prepare_compilation, finalizers: &[] })`.
- Evaluation: `api::js_eval_with_filename` / `api::js_parse_with_filename` + `bytecode` helpers.
- Logging: `JSContext::set_log_func` for stdout hook used by `js_print`.
- Stdlib image: `src/stdlib/mod.rs` exposes `MQUICKJS_STDLIB_IMAGE`.
- Timers and builtin hooks will live in the CLI crate, not core runtime.

## Step-by-step porting plan
1. **Crate scaffolding (done):** add `crates/mquickjs-cli` and register in workspace.
2. **CLI option parser (done):** implement a small parser mirroring `mqjs.c` semantics, with a config struct.
3. **File loading + bytecode dispatch (done):** add `load_file`, detect bytecode, relocate, and execute via `api`.
4. **CLI builtins (done):** port `js_print`, `js_gc`, `js_date_now`, `js_performance_now`, `js_load`, and timers.
5. **scriptArgs injection (done):** mirror `eval_file` behavior for argv passthrough.
6. **REPL core (done):** port `readline` state machine to Rust modules (line edit, history, color).
7. **TTY integration (done):** port `readline_tty` (raw mode, Ctrl-C handling) with platform guards.
8. **Output bytecode path (done):** implement `-o` flow with `prepare_compilation` and 32-bit conversion.
9. **Finalize CLI UX (done):** help text, prompt styling, error formatting, and memory dump parity.

## Test strategy (to port together with implementation)
- Unit tests in `crates/mquickjs-cli` for option parsing and `--memory-limit` parsing.
- Integration tests (Rust) for `-e` and `-o` flows once public API is stable.
- When output diverges, add focused Catch2 tests under `ctests/` that dump C outputs (bytecode, error text).

## Current status
- CLI build + all Rust tests are passing.
- Added integration coverage for `mquickjs-c/tests/mandelbrot.js` (requires `git submodule update --init mquickjs-c`).
