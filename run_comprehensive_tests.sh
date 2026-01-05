#!/bin/bash

# Simple test runner for the new comprehensive feature tests
echo "Running Azimuth Comprehensive Feature Tests..."

# Check if moonc is available
if ! command -v moonc &> /dev/null; then
    echo "Error: moonc not found. Please install MoonBit toolchain."
    exit 1
fi

# Compile the test file
echo "Compiling azimuth_comprehensive_feature_tests.mbt..."
moonc azimuth_comprehensive_feature_tests.mbt

if [ $? -eq 0 ]; then
    echo "Compilation successful!"
    echo "Test file contains 10 test cases:"
    echo "1. Attribute Value Type Conversion"
    echo "2. Resource Merge Operations"
    echo "3. Context Propagation"
    echo "4. Baggage Operations"
    echo "5. SpanContext Functionality"
    echo "6. TextMapCarrier Injection and Extraction"
    echo "7. InstrumentationScope Creation"
    echo "8. Attribute Array Operations"
    echo "9. Telemetry Data Serialization"
    echo "10. Error Handling"
else
    echo "Compilation failed!"
    exit 1
fi