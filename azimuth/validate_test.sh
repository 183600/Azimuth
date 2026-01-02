#!/bin/bash

# Simple validation script for azimuth_advanced_quality_test_suite.mbt

echo "Validating azimuth_advanced_quality_test_suite.mbt..."

# Check if file exists
if [ ! -f "azimuth_advanced_quality_test_suite.mbt" ]; then
    echo "ERROR: Test file not found"
    exit 1
fi

# Check for basic test structure
echo "Checking test file structure..."

# Count test blocks
test_count=$(grep -c "^test " azimuth_advanced_quality_test_suite.mbt)
echo "Found $test_count test cases"

# Check for test function names
test_names=$(grep "^test " azimuth_advanced_quality_test_suite.mbt | sed 's/test "\([^"]*\)".*/\1/')
echo "Test names:"
echo "$test_names"

# Check for assert statements
assert_count=$(grep -c "assert_" azimuth_advanced_quality_test_suite.mbt)
echo "Found $assert_count assert statements"

# Check for proper test syntax
syntax_errors=0

# Check for unmatched quotes
quote_count=$(grep -o '"' azimuth_advanced_quality_test_suite.mbt | wc -l)
if [ $((quote_count % 2)) -ne 0 ]; then
    echo "WARNING: Possible unmatched quotes"
    syntax_errors=$((syntax_errors + 1))
fi

# Check for unmatched braces
open_braces=$(grep -o '{' azimuth_advanced_quality_test_suite.mbt | wc -l)
close_braces=$(grep -o '}' azimuth_advanced_quality_test_suite.mbt | wc -l)
if [ $open_braces -ne $close_braces ]; then
    echo "WARNING: Unmatched braces (open: $open_braces, close: $close_braces)"
    syntax_errors=$((syntax_errors + 1))
fi

# Check for unmatched parentheses
open_parens=$(grep -o '(' azimuth_advanced_quality_test_suite.mbt | wc -l)
close_parens=$(grep -o ')' azimuth_advanced_quality_test_suite.mbt | wc -l)
if [ $open_parens -ne $close_parens ]; then
    echo "WARNING: Unmatched parentheses (open: $open_parens, close: $close_parens)"
    syntax_errors=$((syntax_errors + 1))
fi

# Summary
echo ""
echo "Validation Summary:"
echo "- Test cases: $test_count"
echo "- Assert statements: $assert_count"
echo "- Potential syntax errors: $syntax_errors"

if [ $syntax_errors -eq 0 ] && [ $test_count -gt 0 ] && [ $assert_count -gt 0 ]; then
    echo "✅ Basic validation passed"
    exit 0
else
    echo "❌ Basic validation failed"
    exit 1
fi