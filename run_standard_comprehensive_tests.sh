#!/bin/bash

echo "运行标准综合测试用例..."
echo "测试文件: azimuth/standard_comprehensive_tests.mbt"

cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查测试文件是否存在
if [ ! -f "standard_comprehensive_tests.mbt" ]; then
    echo "错误: 测试文件不存在"
    exit 1
fi

echo "测试文件存在，包含以下测试用例:"
grep -n "test \"" standard_comprehensive_tests.mbt

echo ""
echo "测试用例概述:"
echo "1. basic_addition_functionality - 基本加法功能测试"
echo "2. basic_multiplication_functionality - 基本乘法功能测试"
echo "3. divide_with_ceil_positive_numbers - 向上取整除法正数测试"
echo "4. divide_with_ceil_negative_numbers - 向上取整除法负数测试"
echo "5. greet_function_standard_cases - 问候函数标准测试"
echo "6. subtract_function_basic_cases - 减法函数基本测试"
echo "7. mathematical_commutative_properties - 数学交换律性质测试"
echo "8. business_packaging_scenario - 业务包装场景测试"
echo ""
echo "总计: 8个标准 MoonBit 测试用例"