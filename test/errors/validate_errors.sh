#!/bin/bash
# Validation script for error message tests
# This script validates that error messages contain expected fuzzy matching suggestions

set -e

PROJECT_ROOT="/Users/justin/Code/locque"
cd "$PROJECT_ROOT"

echo "Validating error message tests..."
echo ""

# Test 1: Unqualified name fuzzy matching
echo "Test 1: Unqualified name fuzzy matching"
OUTPUT=$(smyth run test/errors/fuzzy_match_unqualified.lq 2>&1 || true)
if echo "$OUTPUT" | grep -q "Did you mean: my-function?"; then
    echo "  ✓ PASS: Suggests 'my-function' for typo 'my-functoin'"
else
    echo "  ✗ FAIL: Expected suggestion 'my-function' not found"
    echo "  Actual output:"
    echo "$OUTPUT" | sed 's/^/    /'
    exit 1
fi
echo ""

# Test 2: Qualified name fuzzy matching
echo "Test 2: Qualified name fuzzy matching"
OUTPUT=$(smyth run test/errors/fuzzy_match_type.lq 2>&1 || true)
if echo "$OUTPUT" | grep -q "Did you mean: P.add-nat?"; then
    echo "  ✓ PASS: Suggests 'P.add-nat' for typo 'P.add-natt'"
else
    echo "  ✗ FAIL: Expected suggestion 'P.add-nat' not found"
    echo "  Actual output:"
    echo "$OUTPUT" | sed 's/^/    /'
    exit 1
fi
echo ""

echo "All error message tests passed! ✓"
