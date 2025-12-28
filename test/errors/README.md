# Error Message Tests

This directory contains tests for error message quality, particularly fuzzy matching ("Did you mean?") suggestions.

## Test Files

### `fuzzy_match_unqualified.lq`
Tests fuzzy matching for unqualified variable names.

**Typo:** `my-functoin` (should be `my-function`)

**Run:**
```bash
smyth run test/errors/fuzzy_match_unqualified.lq 2>&1
```

**Expected Exit Code:** 1 (failure)

### `fuzzy_match_type.lq`
Tests fuzzy matching for qualified variable names (module prefixes).

**Typo:** `P::add-natt` (should be `P::add-nat`)

**Run:**
```bash
smyth run test/errors/fuzzy_match_type.lq 2>&1
```

**Expected Exit Code:** 1 (failure)

## Validation

These tests are **negative tests** - they are designed to fail with specific error messages. The tests validate that:

1. ✓ Error messages include source location (best-effort)
2. ✓ Error messages clearly state the problem ("Variable not in scope")
3. ✓ Fuzzy matching suggests similar names within Levenshtein distance ≤ 2
4. ✓ Suggestions are concise and helpful
5. ✓ Qualified names (e.g., `P::add-nat`) are matched correctly

## Fuzzy Matching Algorithm

- **Algorithm:** Levenshtein distance (edit distance)
- **Threshold:** Distance ≤ 2
- **Max Suggestions:** Top 3 matches
- **Implementation:** `interpreter/src/ErrorMsg.hs`

## Future Tests

- [ ] Runtime error fuzzy matching (variables not found at runtime)
- [ ] Type mismatch error messages
- [ ] Parser error messages
- [ ] Multiple suggestions (when 2+ names are similar)
- [ ] No suggestions (when no names are close)
- [ ] Case-insensitive matching
