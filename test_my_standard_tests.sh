#!/bin/bash

echo "=== Testing standard_azimuth_tests.mbt ==="

cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "src/azimuth/standard_azimuth_tests.mbt" ]; then
    echo "✓ File exists: src/azimuth/standard_azimuth_tests.mbt"
    
    # 显示文件的前20行
    echo "First 20 lines of the test file:"
    head -20 src/azimuth/standard_azimuth_tests.mbt
    
    echo ""
    echo "Counting test cases in the file..."
    test_count=$(grep -c "^test " src/azimuth/standard_azimuth_tests.mbt)
    echo "Number of test cases: $test_count"
    
    echo ""
    echo "Attempting to run specific test file..."
    
    # 尝试编译并测试特定文件
    ./moon check -pkg-sources azimuth:src/azimuth -single-file src/azimuth/standard_azimuth_tests.mbt 2>&1 | head -20
    
    echo ""
    echo "Attempting to run tests with verbose output..."
    
    # 尝试运行测试并过滤我们的文件
    timeout 30 ./moon test -pkg azimuth 2>&1 | grep -A 10 -B 5 "standard_azimuth_tests" || echo "No specific output found for standard_azimuth_tests"
else
    echo "✗ File not found: src/azimuth/standard_azimuth_tests.mbt"
fi

echo ""
echo "=== Done ==="