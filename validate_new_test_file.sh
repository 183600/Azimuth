#!/bin/bash

# Simple validation script for our new test file
echo "Validating azimuth_azimuth_advanced_comprehensive_tests.mbt..."

# Check if file exists
if [ ! -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_azimuth_advanced_comprehensive_tests.mbt" ]; then
    echo "ERROR: Test file not found!"
    exit 1
fi

# Count test cases
test_count=$(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_azimuth_advanced_comprehensive_tests.mbt)
echo "Found $test_count test cases"

# Check for basic syntax issues
if grep -q "test.*{$" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_azimuth_advanced_comprehensive_tests.mbt; then
    echo "✓ Test syntax looks correct"
else
    echo "ERROR: Test syntax issues found"
    exit 1
fi

# Check for assert statements
assert_count=$(grep -c "assert_" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_azimuth_advanced_comprehensive_tests.mbt)
echo "Found $assert_count assert statements"

echo "Validation complete!"
exit 0