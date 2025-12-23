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
- Added a Rust `build.rs` + `crates/mquickjs-build` generator scaffold that mirrors `mquickjs_build.c` logic (atoms/properties/cfunc metadata) and emits a `stdlib_image.rs` table with tests around core invariants.

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
