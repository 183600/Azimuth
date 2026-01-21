#!/bin/bash
echo "Testing additional_standard_tests.mbt compilation..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth

# 尝试编译我们的测试文件
../../moon check 2>&1 | grep -A 10 -B 10 "additional_standard_tests"

echo ""
echo "Looking for test file in package configuration..."
grep -n "additional_standard_tests" moon.pkg.json