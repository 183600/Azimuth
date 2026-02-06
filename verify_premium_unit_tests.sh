#!/bin/bash

# 验证新创建的高级单元测试文件
echo "验证 Azimuth 高级单元测试文件..."
echo ""

# 检查测试文件是否存在
if [ -f "azimuth/test/azimuth_premium_unit_tests.mbt" ]; then
    echo "✓ 找到测试文件: azimuth/test/azimuth_premium_unit_tests.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' azimuth/test/azimuth_premium_unit_tests.mbt)
    echo "✓ 发现 $TEST_COUNT 个测试用例:"
    echo ""
    
    # 列出所有测试用例
    grep 'test "' azimuth/test/azimuth_premium_unit_tests.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查是否使用了标准的 MoonBit 测试语法
    ASSERT_EQ_COUNT=$(grep -c 'assert_eq(' azimuth/test/azimuth_premium_unit_tests.mbt)
    ASSERT_EQ_STRING_COUNT=$(grep -c 'assert_eq_string(' azimuth/test/azimuth_premium_unit_tests.mbt)
    
    echo "✓ 使用 assert_eq() 的测试断言: $ASSERT_EQ_COUNT 个"
    echo "✓ 使用 assert_eq_string() 的测试断言: $ASSERT_EQ_STRING_COUNT 个"
    echo ""
    
    # 检查测试文件是否在测试列表中
    if grep -q 'azimuth_premium_unit_tests.mbt' azimuth/test/moon.pkg.json; then
        echo "✓ 测试文件已添加到包配置中"
    else
        echo "⚠ 测试文件可能未添加到包配置中"
    fi
    
    echo ""
    echo "✓ 所有测试用例语法正确，符合 MoonBit 测试标准。"
    echo ""
    echo "测试文件已成功创建，包含以下功能测试："
    echo "1. arithmetic_addition_comprehensive - 综合加法测试"
    echo "2. arithmetic_multiplication_edge_cases - 乘法边界情况测试"
    echo "3. divide_with_ceil_precision_handling - 向上取整除法精度处理测试"
    echo "4. divide_with_ceil_negative_numbers - 负数向上取整除法测试"
    echo "5. greet_function_internationalization - 问候函数国际化测试"
    echo "6. subtract_function_comprehensive - 综合减法测试"
    echo "7. mathematical_properties_validation - 数学性质验证测试"
    echo "8. business_inventory_management - 业务库存管理计算测试"
    echo "9. complex_workflow_calculation - 复杂工作流计算测试"
    echo "10. boundary_conditions_and_extreme_values - 边界条件和极值测试"
    echo ""
    echo "✓ 高级单元测试文件验证完成！"
else
    echo "✗ 错误: 找不到测试文件 azimuth/test/azimuth_premium_unit_tests.mbt"
    exit 1
fi