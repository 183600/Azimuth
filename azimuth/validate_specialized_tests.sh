#!/bin/bash

# Simple validation script for the specialized comprehensive tests

echo "Validating azimuth_specialized_comprehensive_tests.mbt..."

# Check if the file exists
if [ ! -f "azimuth_specialized_comprehensive_tests.mbt" ]; then
    echo "Error: Test file does not exist"
    exit 1
fi

# Count the number of tests
test_count=$(grep -c "^test " azimuth_specialized_comprehensive_tests.mbt)
echo "Found $test_count tests in the file"

# Check if test count is <= 10
if [ $test_count -le 10 ]; then
    echo "✓ Test count is within limit (<= 10)"
else
    echo "✗ Test count exceeds limit (> 10)"
    exit 1
fi

# Check for basic syntax elements
if grep -q "test \"" azimuth_specialized_comprehensive_tests.mbt; then
    echo "✓ Test definitions found"
else
    echo "✗ No test definitions found"
    exit 1
fi

if grep -q "assert_" azimuth_specialized_comprehensive_tests.mbt; then
    echo "✓ Assertions found"
else
    echo "✗ No assertions found"
    exit 1
fi

echo "Validation completed successfully!"
exit 0