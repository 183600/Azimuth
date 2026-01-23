#!/bin/bash
echo "验证我们添加的测试用例..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth

# 检查我们的测试用例是否在文件中
echo "检查测试用例是否已添加到 simple_core_tests.mbt:"
grep -n "add_basic_functionality" simple_core_tests.mbt
grep -n "multiply_basic_functionality" simple_core_tests.mbt
grep -n "divide_with_ceil_functionality" simple_core_tests.mbt
grep -n "greet_basic_functionality" simple_core_tests.mbt
grep -n "mathematical_properties" simple_core_tests.mbt
grep -n "negative_number_operations" simple_core_tests.mbt
grep -n "combined_operations" simple_core_tests.mbt
grep -n "practical_applications" simple_core_tests.mbt

echo ""
echo "总测试用例数量:"
grep -c "test " simple_core_tests.mbt

echo ""
echo "测试用例列表:"
grep "test " simple_core_tests.mbt