#!/bin/bash

# Validate new test files
echo "Validating new test files..."

# List of new test files
new_test_files=(
    "azimuth_innovative_telemetry_tests.mbt"
    "azimuth_advanced_edge_case_tests.mbt"
    "azimuth_performance_optimization_tests.mbt"
    "azimuth_internationalization_comprehensive_tests.mbt"
    "azimuth_data_consistency_integrity_tests.mbt"
)

# Check if files exist
for file in "${new_test_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ $file exists"
        
        # Check file size
        size=$(wc -l < "$file")
        echo "  - $size lines"
        
        # Check for test blocks
        test_count=$(grep -c "^test " "$file")
        echo "  - $test_count test cases"
        
        # Basic syntax checks
        if grep -q "test " "$file"; then
            echo "  - Contains test blocks"
        else
            echo "  - WARNING: No test blocks found"
        fi
        
        echo ""
    else
        echo "✗ $file not found"
    fi
done

echo "Validation complete."