# Smyth Tool Integration Tests

This directory contains integration tests for the `smyth` command-line tool.

## Test Files

### `run_basic.lq`
Tests that `smyth run <file>` can execute a simple program successfully.

**Usage:**
```bash
smyth run test/smyth/run_basic.lq
```

**Expected:** Exit code 0, no output

### `assertion_count.lq`
Tests that the assertion counter correctly counts all assertions in a test file.

**Usage:**
```bash
smyth test test/smyth/assertion_count.lq
```

**Expected:** `✓ All tests passed (5 assertions)`

## Running All Smyth Tests

These tests are designed to be run manually as integration tests for the `smyth` tool itself. They validate:

1. **Command execution**: `smyth run` and `smyth test` commands work
2. **Exit codes**: Success (0) and failure (1) exit codes
3. **Assertion counting**: Correct count of assertions
4. **Type checking**: Type checking runs before execution
5. **Error reporting**: Errors are reported clearly

## Future Tests

- [ ] Exit code validation (success vs failure)
- [ ] Type checking integration
- [ ] Parse error reporting
- [ ] Runtime error reporting
- [ ] Project root discovery (Smythfile.lq)
- [ ] Test discovery and organization
