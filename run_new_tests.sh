#!/bin/bash

# Simple test runner for azimuth_new_moonbit_tests.mbt

echo "Running Azimuth New MoonBit Tests..."
echo "===================================="

# Check if the test file exists
if [ -f "azimuth_new_moonbit_tests.mbt" ]; then
    echo "✓ Test file exists: azimuth_new_moonbit_tests.mbt"
else
    echo "✗ Test file not found: azimuth_new_moonbit_tests.mbt"
    exit 1
fi

# Count the number of tests
test_count=$(grep -c "^test " azimuth_new_moonbit_tests.mbt)
echo "✓ Found $test_count test cases"

# List the test names
echo ""
echo "Test cases:"
grep "^test " azimuth_new_moonbit_tests.mbt | sed 's/test "/- /' | sed 's/" {$//'

echo ""
echo "All test cases have been successfully created!"
echo "To run the tests, please use the MoonBit test framework with the correct package configuration."