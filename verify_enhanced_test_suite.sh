#!/bin/bash

echo "=== Verifying azimuth_enhanced_test_suite.mbt ==="

cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "azimuth_enhanced_test_suite.mbt" ]; then
    echo "✓ File exists: azimuth_enhanced_test_suite.mbt"
    
    # 显示文件的前20行
    echo "First 20 lines of the test file:"
    head -20 azimuth_enhanced_test_suite.mbt
    
    echo ""
    echo "Counting test cases in the file..."
    test_count=$(grep -c "^test " azimuth_enhanced_test_suite.mbt)
    echo "Number of test cases: $test_count"
    
    echo ""
    echo "Listing all test cases:"
    grep "^test " azimuth_enhanced_test_suite.mbt
    
    echo ""
    echo "Checking syntax by looking for common patterns..."
    
    # 检查是否有基本的测试语法
    if grep -q "test " azimuth_enhanced_test_suite.mbt; then
        echo "✓ Found test declarations"
    else
        echo "✗ No test declarations found"
    fi
    
    if grep -q "assert_eq" azimuth_enhanced_test_suite.mbt; then
        echo "✓ Found assert_eq assertions"
    else
        echo "✗ No assert_eq assertions found"
    fi
    
    if grep -q "assert_eq_string" azimuth_enhanced_test_suite.mbt; then
        echo "✓ Found assert_eq_string assertions"
    else
        echo "? No assert_eq_string assertions found (may be intentional)"
    fi
    
    echo ""
    echo "Checking for function calls from azimuth library..."
    if grep -q "add\|multiply\|subtract\|divide_with_ceil\|greet" azimuth_enhanced_test_suite.mbt; then
        echo "✓ Found azimuth library function calls"
    else
        echo "✗ No azimuth library function calls found"
    fi
    
    echo ""
    echo "File size and structure analysis:"
    echo "File size: $(wc -l < azimuth_enhanced_test_suite.mbt) lines"
    echo "Number of comments: $(grep -c "//" azimuth_enhanced_test_suite.mbt)"
    echo "Number of empty lines: $(grep -c "^$" azimuth_enhanced_test_suite.mbt)"
    
else
    echo "✗ File not found: azimuth_enhanced_test_suite.mbt"
fi

echo ""
echo "=== Verification Complete ==="