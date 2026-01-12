#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

run_build() {
  echo "==> cabal build (interpreter)"
  (cd "$ROOT/interpreter" && cabal build all)
}

run_test() {
  echo "==> smyth test"
  (cd "$ROOT/interpreter" && cabal build locque-interpreter)
  export LOCQUE_INTERPRETER
  LOCQUE_INTERPRETER="$(cd "$ROOT/interpreter" && cabal list-bin locque-interpreter)"
  (cd "$ROOT/interpreter" && cabal run smyth -- test "$@")
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
  all)
    run_build
    run_test "$@"
    ;;
  *)
    echo "Usage: $0 [build|test|all]" >&2
    exit 2
    ;;
esac
