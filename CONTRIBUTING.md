# Contributing to Locque

## Requirements

- **GHC**: 9.12.2
- **Cabal**: 3.16.x
- **Install via**: [GHCup](https://www.haskell.org/ghcup/)

```bash
ghcup install ghc 9.12.2
ghcup set ghc 9.12.2
ghcup install cabal 3.16.1.0
ghcup set cabal 3.16.1.0
```

## Running Tests

```bash
./tools/ci.sh test
```

This builds the interpreter and runs the full test suite via `smyth test`. To see verbose output:

```bash
./tools/ci.sh test --verbose
```

To run a single test file:

```bash
smyth test test/path/to/file.lq
```

To run benchmarks:

```bash
smyth bench
```

## Code Style

- **Locque source** (`.lq` files): author `.lq` and convert to `.lqs`; do not hand-craft `.lqs` files.
- **Indentation**: 2 spaces per block level; no tabs.
- **`end` alignment**: `end` aligns with the block it closes.
- **Qualification**: use `::` only (e.g., `Module::name`).
- **Comments**: `#` for line comments, `/* */` for block comments in M-expressions.
- **List literals**: use `[]`/`[a, b]` syntax (commas required); use `of-type [] (List A)` when no expected type is in scope.
- **File paths**: `lib/**/*.lq` and `test/**/*.lq` must use lowercase paths matching the module name.
- **Test coverage**: every `lib/**/*.lq` must have a matching `test/**/*.lq`.
- **Haskell interpreter**: follows GHC2021 defaults with `-Wall -Wcompat -Wincomplete-patterns`. HLint is enforced in CI.

## Submission Process

1. Fork the repository at <https://github.com/jaggederest/locque/>.
2. Create a focused branch from `main`.
3. Make your changes; keep commits focused and atomic.
4. Run `./tools/ci.sh test` and ensure all tests pass.
5. Open a pull request against `main` with a clear description of the change.

Do not delete existing tests to make them pass. New primitives require explicit justification.
