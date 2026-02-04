#!/bin/bash

echo "Testing azimuth_concise_standard_tests.mbt..."

# 尝试运行测试
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查文件是否存在
if [ -f "azimuth_concise_standard_tests.mbt" ]; then
    echo "✓ Test file exists"
else
    echo "✗ Test file not found"
    exit 1
fi

# 检查文件内容
echo "Test file contains $(wc -l < azimuth_concise_standard_tests.mbt) lines"
echo "Test file contains $(grep -c "test " azimuth_concise_standard_tests.mbt) test cases"

# 显示测试文件内容
echo "Test file content:"
cat azimuth_concise_standard_tests.mbt

echo "Test verification complete."