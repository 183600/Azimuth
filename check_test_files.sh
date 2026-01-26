#!/bin/bash
echo "Running simple verification test..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth

# 尝试编译并运行单个测试文件
echo "Testing simple_verification_tests.mbt:"
cat simple_verification_tests.mbt

echo ""
echo "Testing azimuth_high_quality_standard_tests.mbt (first few lines):"
head -10 azimuth_high_quality_standard_tests.mbt

echo ""
echo "Checking if files are in package configuration:"
grep -n "simple_verification_tests.mbt" moon.pkg.json
grep -n "azimuth_high_quality_standard_tests.mbt" moon.pkg.json