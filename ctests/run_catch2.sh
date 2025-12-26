#!/usr/bin/env bash
set -euo pipefail

root_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source_dir="${CTESTS_SOURCE_DIR:-${root_dir}/ctests}"
build_dir="${CTESTS_BUILD_DIR:-${source_dir}/build}"
generator="${CTESTS_GENERATOR:-}"

clean=0
configure_only=0
declare -a ctest_args=()

usage() {
  cat <<'EOF'
Usage:
  ctests/run_catch2.sh [--clean] [--configure-only] [--build-dir <dir>] [--source-dir <dir>] [--] [ctest args...]

Examples:
  ctests/run_catch2.sh
  ctests/run_catch2.sh --clean
  ctests/run_catch2.sh -- --output-on-failure -R dtoa
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --clean)
      clean=1
      shift
      ;;
    --configure-only)
      configure_only=1
      shift
      ;;
    --build-dir)
      build_dir="${2:-}"
      shift 2
      ;;
    --source-dir)
      source_dir="${2:-}"
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    --)
      shift
      ctest_args=("$@")
      break
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if ! command -v cmake >/dev/null 2>&1; then
  echo "cmake not found in PATH." >&2
  exit 1
fi
if ! command -v ctest >/dev/null 2>&1; then
  echo "ctest not found in PATH." >&2
  exit 1
fi

if [[ "$clean" -eq 1 ]]; then
  rm -rf "${build_dir}"
fi

configure() {
  if [[ -n "${generator}" ]]; then
    cmake -S "${source_dir}" -B "${build_dir}" -G "${generator}"
  else
    cmake -S "${source_dir}" -B "${build_dir}"
  fi
}

if ! configure; then
  echo "CMake configure failed; retrying from a clean build directory..." >&2
  rm -rf "${build_dir}"
  configure
fi

if [[ "$configure_only" -eq 1 ]]; then
  exit 0
fi

cmake --build "${build_dir}"

if [[ "${#ctest_args[@]}" -eq 0 ]]; then
  ctest_args=(--output-on-failure)
fi
ctest --test-dir "${build_dir}" "${ctest_args[@]}"
