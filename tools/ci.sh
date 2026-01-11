#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

run_build() {
  echo "==> cabal build (interpreter + compiler)"
  (cd "$ROOT/interpreter" && cabal build all)
}

run_test() {
  echo "==> smyth test"
  (cd "$ROOT/interpreter" && cabal run smyth -- test "$@")
}

run_compile_test() {
  echo "==> smyth compile-test"
  (cd "$ROOT/interpreter" && cabal run smyth -- compile-test "$@")
}

run_build_smyth() {
  echo "==> build smyth binary"
  (cd "$ROOT/interpreter" && cabal build smyth)
  SMYTH_BIN="$(cd "$ROOT/interpreter" && cabal list-bin smyth)"
  (cd "$ROOT/smyth" && "$SMYTH_BIN" compile --out "$ROOT/smyth/tmp/locque/bin/smyth" lib/main.lq)
}

mode="${1:-all}"
shift || true
case "$mode" in
  build)
    run_build
    ;;
  test)
    run_test "$@"
    ;;
  compile-test)
    run_compile_test "$@"
    ;;
  build-smyth)
    run_build_smyth
    ;;
  all)
    run_build
    run_test "$@"
    run_compile_test "$@"
    run_build_smyth
    ;;
  *)
    echo "Usage: $0 [build|test|compile-test|build-smyth|all]" >&2
    exit 2
    ;;
esac
