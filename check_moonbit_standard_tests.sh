#!/bin/bash

echo "=== Testing moonbit_standard_tests.mbt ==="

cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "test/moonbit_standard_tests.mbt" ]; then
    echo "✓ File exists: test/moonbit_standard_tests.mbt"
    
    # 显示文件的前20行
    echo "First 20 lines of the test file:"
    head -20 test/moonbit_standard_tests.mbt
    
    echo ""
    echo "Counting test cases in the file..."
    test_count=$(grep -c "^test " test/moonbit_standard_tests.mbt)
    echo "Number of test cases: $test_count"
    
    echo ""
    echo "Checking test configuration..."
    
    # 检查我们的文件是否在配置中
    if grep -q "moonbit_standard_tests.mbt" test/moon.pkg.json; then
        echo "✓ File found in test configuration"
        echo "Lines containing moonbit_standard_tests.mbt in moon.pkg.json:"
        grep -n "moonbit_standard_tests.mbt" test/moon.pkg.json
    else
        echo "✗ File not found in test configuration"
    fi
    
    echo ""
    echo "Attempting to run tests..."
    
    # 尝试运行测试并过滤我们的文件
    timeout 10 ./moon test 2>&1 | grep -A 5 -B 5 "moonbit_standard_tests" || echo "No output found for moonbit_standard_tests"
else
    echo "✗ File not found: test/moonbit_standard_tests.mbt"
fi

echo ""
echo "=== Done ==="