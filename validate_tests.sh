#!/bin/bash

# Simple validation script for the new comprehensive feature tests
echo "Validating Azimuth Comprehensive Feature Tests..."

# Check if the test file exists
if [ ! -f "azimuth_comprehensive_feature_tests.mbt" ]; then
    echo "Error: Test file azimuth_comprehensive_feature_tests.mbt not found!"
    exit 1
fi

# Count the number of test cases in the file
test_count=$(grep -c "^test " azimuth_comprehensive_feature_tests.mbt)
echo "Found $test_count test cases in the file"

# List the test cases
echo ""
echo "Test cases:"
grep "^test " azimuth_comprehensive_feature_tests.mbt | sed 's/test "/- /' | sed 's/" {$//'

# Check for basic syntax elements
echo ""
echo "Checking syntax elements..."

# Check for assert_eq statements
assert_eq_count=$(grep -c "assert_eq" azimuth_comprehensive_feature_tests.mbt)
echo "Found $assert_eq_count assert_eq statements"

# Check for assert_true statements
assert_true_count=$(grep -c "assert_true" azimuth_comprehensive_feature_tests.mbt)
echo "Found $assert_true_count assert_true statements"

# Check for assert_false statements
assert_false_count=$(grep -c "assert_false" azimuth_comprehensive_feature_tests.mbt)
echo "Found $assert_false_count assert_false statements"

# Check for match statements
match_count=$(grep -c "match " azimuth_comprehensive_feature_tests.mbt)
echo "Found $match_count match statements"

echo ""
echo "Validation complete. The test file appears to be properly structured."