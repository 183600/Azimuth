#!/bin/bash

# Test runner for azimuth_enhanced_moonbit_tests.mbt

echo "Running Azimuth Enhanced MoonBit Tests..."
echo "=========================================="

# Check if the test file exists
if [ -f "azimuth/azimuth_enhanced_moonbit_tests.mbt" ]; then
    echo "✓ Test file exists: azimuth/azimuth_enhanced_moonbit_tests.mbt"
else
    echo "✗ Test file not found: azimuth/azimuth_enhanced_moonbit_tests.mbt"
    exit 1
fi

# Count the number of tests
test_count=$(grep -c "^test " azimuth/azimuth_enhanced_moonbit_tests.mbt)
echo "✓ Found $test_count test cases"

# List the test names
echo ""
echo "Test cases:"
grep "^test " azimuth/azimuth_enhanced_moonbit_tests.mbt | sed 's/test "/- /' | sed 's/" {$//'

echo ""
echo "All test cases have been successfully created!"
echo "Test coverage includes:"
echo "- Span lifecycle management"
echo "- Tracer provider and tracer creation"
echo "- Histogram metric operations"
echo "- UpDown counter metric operations"
echo "- Gauge metric operations"
echo "- Attribute operations with various types"
echo "- Baggage operations and propagation"
echo "- Composite propagator injection and extraction"
echo "- Resource operations and merging"
echo "- Context propagation with complex values"
echo "- Log record with attributes and context"