#!/bin/bash

# Validation script for newly created test files
echo "Validating Newly Created Azimuth Test Files"
echo "=========================================="

# List of newly created test files
new_test_files=(
    "azimuth_advanced_time_series_operations_tests.mbt"
    "azimuth_realtime_telemetry_processing_tests.mbt"
    "azimuth_boundary_condition_error_handling_tests.mbt"
    "azimuth_performance_benchmark_tests.mbt"
    "azimuth_concurrent_safety_tests.mbt"
    "azimuth_data_integrity_validation_tests.mbt"
    "azimuth_telemetry_configuration_management_tests.mbt"
    "azimuth_cross_service_consistency_tests.mbt"
    "azimuth_internationalization_globalization_tests.mbt"
    "azimuth_resource_limit_recovery_tests.mbt"
)

total_tests=0
valid_files=0

echo ""
echo "Checking test files..."
echo "-------------------"

for file in "${new_test_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ Found: $file"
        
        # Count test cases in the file
        test_count=$(grep -c "^test " "$file")
        echo "  - Test cases: $test_count"
        
        # Check for basic syntax (simple checks)
        if grep -q "^test " "$file" && grep -q "assert_" "$file"; then
            echo "  - Syntax: Valid"
            valid_files=$((valid_files + 1))
        else
            echo "  - Syntax: Needs review"
        fi
        
        total_tests=$((total_tests + test_count))
        echo ""
    else
        echo "✗ Missing: $file"
        echo ""
    fi
done

echo "Validation Summary"
echo "================"
echo "Total files checked: ${#new_test_files[@]}"
echo "Valid files: $valid_files"
echo "Total test cases: $total_tests"

if [ $valid_files -eq ${#new_test_files[@]} ]; then
    echo "✓ All test files are valid!"
    exit 0
else
    echo "✗ Some test files need attention."
    exit 1
fi