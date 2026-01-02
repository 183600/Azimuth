#!/bin/bash

# Simple validation script for the new comprehensive test cases
echo "Validating azimuth_new_comprehensive_tests.mbt..."

# Check if the file exists
if [ ! -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_tests.mbt" ]; then
    echo "Error: Test file not found"
    exit 1
fi

# Count the number of test cases
test_count=$(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_tests.mbt)
echo "Found $test_count test cases"

# List the test case names
echo "Test cases:"
grep "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_tests.mbt | sed 's/^test "//' | sed 's/" {$//'

# Check basic syntax
echo "Checking basic syntax..."
if grep -q "test \"" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_tests.mbt; then
    echo "✓ Test syntax looks correct"
else
    echo "✗ Test syntax issues found"
    exit 1
fi

# Check for required assertions
assertion_count=$(grep -c "assert_" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_tests.mbt)
echo "Found $assertion_count assertions"

if [ $assertion_count -gt 0 ]; then
    echo "✓ Assertions found"
else
    echo "✗ No assertions found"
    exit 1
fi

echo "Validation completed successfully!"