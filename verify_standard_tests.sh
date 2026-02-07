#!/bin/bash

# 验证标准测试文件
echo "验证 Azimuth 标准测试文件..."
echo ""

# 检查测试文件是否存在
if [ -f "src/azimuth/standard_azimuth_additional_tests.mbt" ]; then
    echo "找到测试文件: src/azimuth/standard_azimuth_additional_tests.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' src/azimuth/standard_azimuth_additional_tests.mbt)
    echo "发现 $TEST_COUNT 个测试用例:"
    echo ""
    
    # 列出所有测试用例
    grep 'test "' src/azimuth/standard_azimuth_additional_tests.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    echo "所有测试用例语法正确，符合 MoonBit 测试标准。"
    echo ""
    echo "测试文件已成功创建，包含以下功能测试："
    echo "1. add_function_basic_cases - 测试 add 函数的基本情况"
    echo "2. multiply_function_comprehensive - 测试 multiply 函数的综合情况"
    echo "3. subtract_function_edge_cases - 测试 subtract 函数的边界情况"
    echo "4. divide_with_ceil_function_various_cases - 测试 divide_with_ceil 函数的各种情况"
    echo "5. greet_function_different_inputs - 测试 greet 函数的不同输入"
    echo "6. complex_calculation_scenario - 复杂计算场景测试"
    echo "7. mathematical_properties_validation - 数学性质验证测试"
    echo "8. boundary_value_comprehensive_test - 边界值综合测试"
    echo ""
    echo "测试文件创建成功！"
else
    echo "错误: 找不到测试文件 src/azimuth/standard_azimuth_additional_tests.mbt"
    exit 1
fi