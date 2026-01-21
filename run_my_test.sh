#!/bin/bash

echo "Running standard_functionality_tests.mbt..."

# 切换到azimuth目录
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 尝试编译我们的测试文件
echo "Compiling test file..."
../moon check test/standard_functionality_tests.mbt

# 尝试运行测试
echo "Running test..."
../moon test 2>&1 | grep -A 20 "standard_functionality_tests.mbt"