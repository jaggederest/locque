# Test Coverage Audit

## Completed Features from TODO.md

### ✓ Type System Features (TESTED)
- [x] Bidirectional type checking - `test/typecheck/fold_inline.lq`, `test/typecheck/mono.lq`
- [x] Type-specific match primitives - `test/typecheck/match.lq`, `test/features/match.lq`, `test/features/match_semantics.lq`
- [x] Fresh variable generation - `test/typecheck/opaque_import.lq`
- [x] Opaque imports - `test/typecheck/opaque_import.lq`

### ✓ Language Features (TESTED)
- [x] Basic expressions - `test/features/basics.lq`
- [x] File I/O - `test/features/file_io.lq`
- [x] Lambda functions - `test/features/lambda.lq`
- [x] Fold/map operations - `test/features/fold.lq`
- [x] Match primitives - `test/features/match.lq`, `test/features/match_semantics.lq`
- [x] Let bindings - `test/features/let_binding.lq`
- [x] Primitives - `test/features/primitives.lq`, `test/features/new_primitives.lq`
- [x] Imports - `test/features/imports.lq`
- [x] Arithmetic - `test/features/arithmetic.lq`
- [x] Comparison - `test/features/comparison.lq`
- [x] String utilities - `test/features/string_utils.lq`

### ⚠️ Language Features (PARTIALLY TESTED)
- [ ] Comment syntax (M-expr: `#`, `/* */`, S-expr: `;`, `#| |#`) - **MISSING TESTS**
- [ ] List.slice function - **NEED TO VERIFY COVERAGE**

### ❌ Tooling Features (NOT TESTED)
- [ ] `smyth test` command - **NO INTEGRATION TESTS**
- [ ] `smyth run` command - **NO INTEGRATION TESTS**
- [ ] Assertion counting (115 assertions) - **NO TESTS VALIDATING COUNT**
- [ ] Standalone binary generation - **NO TESTS**
- [ ] Smythfile.lq project configuration - **NO TESTS**
- [ ] Test organization (modular structure) - **NO TESTS**
- [ ] Exit codes (0=pass, 1=fail) - **NO TESTS**

### ❌ Error Message Improvements (NOT TESTED)
- [ ] Fuzzy matching ("did you mean?") - **NO TESTS**
- [ ] Levenshtein distance algorithm - **NO TESTS**
- [ ] Enhanced TypeError with location - **NO TESTS**
- [ ] Runtime error suggestions - **NO TESTS**
- [ ] Error message token efficiency - **NO TESTS**

### ❌ Conversion Features (NOT TESTED)
- [ ] M-expr ↔ S-expr roundtrip - `test/51_roundtrip.lq` exists but **NEEDS VERIFICATION**
- [ ] `--emit-lqs` command - **NO TESTS**
- [ ] `--emit-lq` command - **NO TESTS**

### ❌ Validation Features (NOT TESTED)
- [ ] Parenthesis checking - `test/40_small_bad_code.lqs` exists but **NEEDS VERIFICATION**
- [ ] Structural validation - **NO TESTS**
- [ ] `validate-prim` primitive - **NO TESTS**

## Test Gaps by Priority

### High Priority (Core Functionality)
1. **smyth test** - Integration tests for test runner
2. **smyth run** - Integration tests for file runner
3. **Error messages** - Tests for fuzzy matching and suggestions
4. **Assertion counting** - Verify assertion count is correct

### Medium Priority (Quality of Life)
5. **Comment syntax** - Test both M-expr and S-expr comments
6. **Conversion** - Test M-expr ↔ S-expr roundtrips
7. **Exit codes** - Test success/failure exit codes
8. **Smythfile.lq** - Test project root discovery

### Low Priority (Edge Cases)
9. **Validation** - Test validator error messages
10. **Standalone binary** - Test binary installation
11. **List.slice** - Verify coverage in existing tests

## Recommended Test Structure

```
test/
├── features/       # Language feature tests (EXISTS)
├── typecheck/      # Type checker tests (EXISTS)
├── smyth/          # Smyth tool integration tests (TO CREATE)
│   ├── test_runner.lq      # Test `smyth test`
│   ├── file_runner.lq      # Test `smyth run`
│   ├── exit_codes.lq       # Test exit codes
│   ├── assertion_count.lq  # Test assertion counting
│   └── project_root.lq     # Test Smythfile.lq discovery
├── errors/         # Error message tests (TO CREATE)
│   ├── fuzzy_match.lq      # Test "did you mean?" suggestions
│   ├── type_errors.lq      # Test enhanced type errors
│   └── runtime_errors.lq   # Test runtime error suggestions
├── conversion/     # Conversion tests (TO CREATE)
│   ├── roundtrip.lq        # Test M-expr ↔ S-expr
│   ├── emit_lqs.lq         # Test --emit-lqs
│   └── emit_lq.lq          # Test --emit-lq
└── syntax/         # Syntax tests (TO CREATE)
    ├── comments.lq         # Test comment syntax
    └── validation.lq       # Test validator
```

## Action Items

1. Create `test/smyth/` directory
2. Create `test/errors/` directory
3. Create `test/conversion/` directory
4. Create `test/syntax/` directory
5. Write comprehensive tests for each gap
6. Update this document as tests are added
7. Add test count to CI/CD validation
