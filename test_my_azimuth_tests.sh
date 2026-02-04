#!/bin/bash

echo "=== Testing azimuth_standard_enhanced_tests.mbt ==="

cd /home/runner/work/Azimuth/Azimuth/test

# 检查文件是否存在
if [ -f "azimuth_standard_enhanced_tests.mbt" ]; then
    echo "✓ File exists: azimuth_standard_enhanced_tests.mbt"
    
    # 显示文件的前20行
    echo "First 20 lines of the test file:"
    head -20 azimuth_standard_enhanced_tests.mbt
    
    echo ""
    echo "Counting test cases in the file..."
    test_count=$(grep -c "^test " azimuth_standard_enhanced_tests.mbt)
    echo "Number of test cases: $test_count"
    
    echo ""
    echo "Checking test configuration..."
    
    # 检查我们的文件是否在配置中
    if grep -q "azimuth_standard_enhanced_tests.mbt" moon.pkg.json; then
        echo "✓ File found in test configuration"
        echo "Lines containing azimuth_standard_enhanced_tests.mbt in moon.pkg.json:"
        grep -n "azimuth_standard_enhanced_tests.mbt" moon.pkg.json
    else
        echo "✗ File not found in test configuration"
    fi
    
    echo ""
    echo "Attempting to run tests..."
    
    # 尝试运行测试并过滤我们的文件
    timeout 10 ../moon test 2>&1 | grep -A 5 -B 5 "azimuth_standard_enhanced_tests" || echo "No specific output found for azimuth_standard_enhanced_tests"
    
    echo ""
    echo "Checking for any compilation errors related to our file..."
    timeout 10 ../moon test 2>&1 | grep -i "azimuth_standard_enhanced_tests" || echo "No compilation errors found for our file"
else
    echo "✗ File not found: azimuth_standard_enhanced_tests.mbt"
fi

echo ""
echo "=== Done ==="