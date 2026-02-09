#!/bin/bash

echo "=== Testing azimuth_standard_core_tests.mbt ==="

cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "azimuth/azimuth_standard_core_tests.mbt" ]; then
    echo "✓ File exists: azimuth/azimuth_standard_core_tests.mbt"
    
    # 显示文件的前20行
    echo "First 20 lines of the test file:"
    head -20 azimuth/azimuth_standard_core_tests.mbt
    
    echo ""
    echo "Counting test cases in the file..."
    test_count=$(grep -c "^test " azimuth/azimuth_standard_core_tests.mbt)
    echo "Number of test cases: $test_count"
    
    echo ""
    echo "Checking test configuration..."
    
    # 检查我们的文件是否在配置中
    if grep -q "azimuth_standard_core_tests.mbt" azimuth/moon.pkg.json; then
        echo "✓ File found in test configuration"
        echo "Lines containing azimuth_standard_core_tests.mbt in moon.pkg.json:"
        grep -n "azimuth_standard_core_tests.mbt" azimuth/moon.pkg.json
    else
        echo "✗ File not found in test configuration"
    fi
    
    echo ""
    echo "Checking syntax by examining test patterns..."
    
    # 检查测试语法
    echo "Checking test syntax patterns:"
    echo "1. Test declarations:"
    grep -n "^test " azimuth/azimuth_standard_core_tests.mbt
    
    echo ""
    echo "2. Function calls with @azimuth prefix:"
    grep -n "@azimuth\." azimuth/azimuth_standard_core_tests.mbt
    
    echo ""
    echo "3. Assert patterns:"
    grep -n "assert_eq" azimuth/azimuth_standard_core_tests.mbt
    grep -n "assert_eq_string" azimuth/azimuth_standard_core_tests.mbt
    
else
    echo "✗ File not found: azimuth/azimuth_standard_core_tests.mbt"
fi

echo ""
echo "=== Done ==="