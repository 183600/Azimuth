#!/bin/bash

# Validation script for enhanced feature tests
echo "Running enhanced feature tests validation..."

# Check if the test file exists
if [ ! -f "azimuth_enhanced_feature_tests.mbt" ]; then
    echo "Error: azimuth_enhanced_feature_tests.mbt not found!"
    exit 1
fi

echo "✓ Test file exists"

# Check if the test file contains the expected test cases
expected_tests=(
    "time series data processing"
    "distributed tracing consistency"
    "performance optimization and resource management"
    "error handling and recovery mechanisms"
    "data serialization and deserialization"
    "internationalization support"
    "platform compatibility"
    "advanced metrics aggregation"
)

for test in "${expected_tests[@]}"; do
    if grep -q "test \"$test\"" azimuth_enhanced_feature_tests.mbt; then
        echo "✓ Test case '$test' found"
    else
        echo "✗ Test case '$test' not found"
        exit 1
    fi
done

# Check syntax by attempting to compile (if moon compiler is available)
if command -v moon &> /dev/null; then
    echo "Attempting to compile the test file..."
    if moon check azimuth_enhanced_feature_tests.mbt; then
        echo "✓ Test file compiles successfully"
    else
        echo "✗ Test file compilation failed"
        exit 1
    fi
else
    echo "Moon compiler not found, skipping compilation check"
fi

echo "Enhanced feature tests validation completed successfully!"
echo "Created 8 new test cases covering advanced telemetry features:"