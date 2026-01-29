#!/bin/bash

echo "=== Testing standard_moonbit_tests.mbt ==="

cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "src/azimuth/standard_moonbit_tests.mbt" ]; then
    echo "✓ File exists: src/azimuth/standard_moonbit_tests.mbt"
    
    # 显示文件的前20行
    echo "First 20 lines of the test file:"
    head -20 src/azimuth/standard_moonbit_tests.mbt
    
    echo ""
    echo "Counting test cases in the file..."
    test_count=$(grep -c "^test " src/azimuth/standard_moonbit_tests.mbt)
    echo "Number of test cases: $test_count"
    
    echo ""
    echo "Checking test configuration..."
    
    # 检查我们的文件是否在配置中
    if grep -q "standard_moonbit_tests.mbt" src/azimuth/moon.pkg.json; then
        echo "✓ File found in test configuration"
        echo "Lines containing standard_moonbit_tests.mbt in moon.pkg.json:"
        grep -n "standard_moonbit_tests.mbt" src/azimuth/moon.pkg.json
    else
        echo "✗ File not found in test configuration"
        echo "Adding file to test configuration..."
        # 这里可以添加文件到配置的命令
    fi
    
    echo ""
    echo "Displaying test case names:"
    grep "^test " src/azimuth/standard_moonbit_tests.mbt
    
else
    echo "✗ File not found: src/azimuth/standard_moonbit_tests.mbt"
fi

echo ""
echo "=== Done ==="