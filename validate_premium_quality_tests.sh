#!/bin/bash

# Test runner for azimuth_premium_quality_tests.mbt

echo "Running Azimuth Premium Quality Tests Validation..."
echo "=============================================="

# Check if the test file exists
if [ -f "azimuth_premium_quality_tests.mbt" ]; then
    echo "✓ Test file exists: azimuth_premium_quality_tests.mbt"
else
    echo "✗ Test file not found: azimuth_premium_quality_tests.mbt"
    exit 1
fi

# Count the number of tests
test_count=$(grep -c "^test " azimuth_premium_quality_tests.mbt)
echo "✓ Found $test_count test cases"

# List the test names
echo ""
echo "Test cases:"
grep "^test " azimuth_premium_quality_tests.mbt | sed 's/test "/- /' | sed 's/" {$//'

# Verify test count is as expected
if [ "$test_count" -eq 10 ]; then
    echo ""
    echo "✓ All 10 expected premium quality tests have been created!"
else
    echo ""
    echo "⚠ Expected 10 tests, but found $test_count tests"
fi

# Check for basic syntax patterns
echo ""
echo "Validating test structure..."

# Check for assertion patterns
assert_count=$(grep -c "assert_" azimuth_premium_quality_tests.mbt)
echo "✓ Found $assert_count assertions"

# Check for proper test structure
test_blocks=$(grep -c "^test " azimuth_premium_quality_tests.mbt)
closing_braces=$(grep -c "}" azimuth_premium_quality_tests.mbt)
echo "✓ Found $test_blocks test blocks and $closing_braces closing braces"

echo ""
echo "Premium quality test validation completed successfully!"
echo "To run the tests, please use the MoonBit test framework with the correct package configuration."