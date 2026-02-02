#!/bin/bash

echo "=== Testing azimuth_core_standard_tests.mbt ==="

cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "src/azimuth/azimuth_core_standard_tests.mbt" ]; then
    echo "✓ File exists: src/azimuth/azimuth_core_standard_tests.mbt"
    
    # 显示文件的前20行
    echo "First 20 lines of the test file:"
    head -20 src/azimuth/azimuth_core_standard_tests.mbt
    
    echo ""
    echo "Counting test cases in the file..."
    test_count=$(grep -c "^test " src/azimuth/azimuth_core_standard_tests.mbt)
    echo "Number of test cases: $test_count"
    
    echo ""
    echo "Checking test configuration..."
    
    # 检查我们的文件是否在配置中
    if grep -q "azimuth_core_standard_tests.mbt" src/azimuth/moon.pkg.json; then
        echo "✓ File found in test configuration"
        echo "Lines containing azimuth_core_standard_tests.mbt in moon.pkg.json:"
        grep -n "azimuth_core_standard_tests.mbt" src/azimuth/moon.pkg.json
    else
        echo "✗ File not found in test configuration"
    fi
    
    echo ""
    echo "Displaying test case names:"
    grep "^test " src/azimuth/azimuth_core_standard_tests.mbt
    
else
    echo "✗ File not found: src/azimuth/azimuth_core_standard_tests.mbt"
fi

echo ""
echo "=== Done ==="