# Locque COMPLETED

## Performance: project/shell suites
- Added `walk-filter-prim` and `File::walk-filter`/`File::walk-lq` to reduce file traversal and filtering overhead.
- Fixed `walk-filter-prim` skip-prefix handling so roots are not incorrectly skipped.
- Reworked `test/project.lq` to avoid `Character` refinement in uppercase checks, use dictionary-backed membership, and replace split/join prefix stripping.
- Consolidated `smyth dump` usage in shell tests (multi-file bundling) to reduce process launches.
- Moved fuzzy-match checks into `Smythfile.lq` error-tests to remove `smyth run` subprocesses from shell suite.
- Added `smyth run --pid-file` and `--timeout` and updated `test/shell/lhttp.lq` to avoid `ps|grep|awk` polling.
