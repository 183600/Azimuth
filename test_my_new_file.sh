#!/bin/bash

echo "Testing moon_comprehensive_tests.mbt..."

cd /home/runner/work/Azimuth/Azimuth/azimuth

# 尝试直接运行测试
./moon test 2>&1 | grep -A 20 "moon_comprehensive_tests"

# 如果没有找到，尝试检查文件是否存在
if [ -f "test/moon_comprehensive_tests.mbt" ]; then
    echo "Test file exists at test/moon_comprehensive_tests.mbt"
    
    # 检查文件内容
    echo "First few lines of the test file:"
    head -10 test/moon_comprehensive_tests.mbt
else
    echo "Test file not found!"
fi