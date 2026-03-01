# Plan: Clean Up Duplicate and Redundant Code

## Summary

The compiler was already removed (commit d9839bb, Jan 2026) with no active remnants.
However, significant **code duplication** exists across Haskell interpreter modules.
This plan consolidates duplicated functions, eliminates the redundant
`locque-interpreter` executable (whose functionality is a strict subset of `smyth`),
and deduplicates internal logic within SmythDump.hs.

---

## Step 1: Consolidate AST utility functions into a shared module

**Problem:** Several pure AST utility functions are copy-pasted across 2-4 files each.

**Create `interpreter/src/ASTUtils.hs`** with these functions (currently duplicated):

| Function | Currently in | Copies |
|---|---|---|
| `stripExpr` | SmythConfig, Recursor, (DictPass as `stripType`) | 3 |
| `isEffectAny` | Type, Parser, DictPass, TypeChecker | 4 |
| `typeConstName` | Type, DictPass | 2 |
| `collectApp` | Recursor, TypeChecker | 2 |
| `freeVars` / `freeVarsWithBound` | Recursor, TypeChecker | 2 (~90 lines each) |
| `renameBound` | Recursor, TypeChecker, DictPass | 3 |
| `freshName` | Recursor, DictPass | 2 |
| `splitForAll` | Recursor, TypeChecker | 2 |
| `mkApp` | Recursor (only, but useful shared) | 1 |

**Actions:**
- Move canonical definitions into `ASTUtils.hs`, exported from there
- For `freshName`: standardize on one separator (hyphen, matching Recursor)
- For `renameBound`: use the binding-aware version from TypeChecker/Recursor
- For `stripType` in DictPass: replace with import of `stripExpr`
- Update all importing modules to use `ASTUtils`
- Remove duplicate definitions from SmythConfig, Recursor, DictPass, TypeChecker, Parser, Type
- Keep `typeConstName` also exported from Type.hs (re-export from ASTUtils) for backward compat

**Files changed:** ASTUtils.hs (new), SmythConfig.hs, Recursor.hs, DictPass.hs,
TypeChecker.hs, Parser.hs, Type.hs

---

## Step 2: Consolidate small utility functions

**Problem:** `resolvePath` and `listLqFiles` are duplicated across Smyth modules.

**Expand `Utils.hs`** with:

| Function | Currently in | Copies |
|---|---|---|
| `resolvePath` | SmythBench, SmythTest | 2 |
| `listLqFiles` | SmythCount, SmythDependencies | 2 |

**Actions:**
- Move `resolvePath` and `listLqFiles` into `Utils.hs`
- Update SmythBench, SmythTest, SmythCount, SmythDependencies to import from Utils

**Files changed:** Utils.hs, SmythBench.hs, SmythTest.hs, SmythCount.hs,
SmythDependencies.hs

---

## Step 3: Eliminate `locque-interpreter` executable (Main.hs)

**Problem:** `Main.hs` (the `locque-interpreter` executable) duplicates functionality
that already exists in the `smyth` tool modules:

- `Main.hs:runFile` duplicates `SmythRun.hs:runFileNoExit` (but simpler, no caching)
- `Main.hs:dumpFile` duplicates `SmythDump.hs:dumpFile` (~60 lines, nearly identical)
- `Main.hs:parseAny` duplicates `SmythDump.hs:parseAny` (identical)
- `Main.hs:selectModule` duplicates `SmythDump.hs:selectModule` (identical)
- `Main.hs:dumpTypes` duplicates `SmythDump.hs:dumpTypes` (identical)
- `Main.hs:typecheckFile` has no Smyth equivalent but is trivial
- `Main.hs:validateLqs` has no Smyth equivalent but is trivial
- `Main.hs:emitLqs`/`emitLq` have no Smyth equivalent but are trivial

**Actions:**
- Rewrite `Main.hs` to delegate to Smyth modules for shared functionality
  (`runFile` → `SmythRun`, dump → `SmythDump`)
- Keep `locque-interpreter` executable for backward compat but make it thin
- Move `parseAny`, `selectModule`, `dumpTypes` out of SmythDump.hs into a shared
  location (either SmythDump exports them, or a small shared module)
- Remove all duplicated function bodies from Main.hs

**Files changed:** Main.hs (major rewrite to thin wrapper), SmythDump.hs (export shared
functions), locque-interpreter.cabal (update module list)

---

## Step 4: Deduplicate SmythDump.hs internal logic

**Problem:** `SmythDump.hs` has `dumpFile` (lines 134-195) and `dumpMode` (lines 75-132)
which both implement the same 7-mode switch over dump modes. `dumpFile` is the single-file
path and `dumpMode` is the `--multi` path, but they share identical case logic.

**Actions:**
- Refactor `dumpFile` to call `dumpMode` (or a shared inner function) for the actual
  mode dispatch, eliminating the duplicated switch
- This removes ~60 lines of duplicated case matching

**Files changed:** SmythDump.hs

---

## Step 5: Consolidate run pipeline between SmythRun and SmythTest

**Problem:** `SmythRun.hs:runFileNoExit` and `SmythTest.hs:runTestOutcome` implement
nearly identical pipelines:
```
parse → digest → cache check → typecheck+normalize → insertRecursors →
annotate → dictPass transform → cache write → run
```

**Actions:**
- Extract the shared pipeline into a function in `SmythRun.hs` (e.g.,
  `runPipeline :: FilePath -> FilePath -> IO (Either Failure (CtorArityMap, Module))`)
  that handles: digest, cache, typecheck, normalize, recursors, annotate, transform,
  cache write
- Have `SmythTest.hs:runTestOutcome` call the shared pipeline instead of reimplementing it
- Keep the timed variant (`runTestOutcomeTimed`) as a SmythTest-specific wrapper

**Files changed:** SmythRun.hs (extract pipeline), SmythTest.hs (use shared pipeline)

---

## Step 6: Update cabal file and run tests

**Actions:**
- Add `ASTUtils` to all three executable `other-modules` lists in
  `locque-interpreter.cabal`
- Ensure `Utils` additions are reflected if not already listed
- Run `smyth test` to verify nothing is broken
- Run `cabal build all` to ensure all three executables compile

**Files changed:** locque-interpreter.cabal

---

## What is NOT in scope

- Splitting large files (TypeChecker.hs at 3451 lines, Parser.hs at 1454 lines,
  Eval.hs at 1387 lines) - these are large but not duplicated; splitting is a
  separate task
- Removing the `locque-interpreter` executable entirely (keeping for backward compat)
- Changes to `.lq` library/test files
- Changes to LocqueLsp.hs (it has its own type-checking logic but serves a different
  purpose)

## Estimated impact

- ~300+ lines of duplicated code removed
- 1 new file (`ASTUtils.hs`, ~200 lines consolidating what was ~400+ across files)
- Net reduction of ~200+ lines
- Clearer module boundaries and single source of truth for AST utilities
