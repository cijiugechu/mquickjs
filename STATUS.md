# Status

## MQuickJS-C leaf modules

Based on local `#include "..."` dependencies (considering both `.c` and `.h`), the leaf modules are:

- `mquickjs-c/cutils.c` + `mquickjs-c/cutils.h`
- `mquickjs-c/list.h`
- `mquickjs-c/mquickjs_opcode.h`
- `mquickjs-c/softfp_template_icvt.h`

## Porting progress

- Added `intrusive-collections = "0.9.7"` to model the `list.h` intrusive list pattern.
- Ported `mquickjs-c/list.h` into `src/list.rs` with tests covering insertion, iteration, and removal.
- Ported `mquickjs-c/mquickjs_opcode.h` into `src/opcode.rs` with metadata tables and ordering invariant tests.
- Confirmed the `#if 0` opcodes in `mquickjs-c/mquickjs_opcode.h` are unused in both build and runtime code; only a debug-printing branch references them and is also disabled, so Rust should ignore them unless a future feature flag is desired.
