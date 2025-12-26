# Testing

## Rust tests

Run the full Rust test suite and lint checks:

```bash
cargo check --all-features
cargo test --all-features
cargo clippy
```

## C tests (Catch2 + CMake)

Catch2 tests are built via CMake and registered with CTest:

```bash
cmake -S ctests -B ctests/build
cmake --build ctests/build
ctest --test-dir ctests/build
```

Notes:
- Requires CMake 3.20+.
- Uses `FetchContent` to download Catch2 on first configure.

### One-shot script

Use `ctests/run_catch2.sh` to configure, build, and run tests with a retry on
configure failures:

```bash
./ctests/run_catch2.sh
./ctests/run_catch2.sh --clean
./ctests/run_catch2.sh -- --output-on-failure -R dtoa
```
