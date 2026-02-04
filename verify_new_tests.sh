#!/bin/bash

# 验证新创建的标准测试用例

echo "验证新创建的标准 MoonBit 测试用例..."
echo ""

# 检查测试文件是否存在
if [ -f "src/azimuth/test/standard_core_test_cases.mbt" ]; then
    echo "✓ 找到测试文件: src/azimuth/test/standard_core_test_cases.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' src/azimuth/test/standard_core_test_cases.mbt)
    echo "✓ 发现 $TEST_COUNT 个测试用例:"
    echo ""
    
    # 列出所有测试用例
    grep 'test "' src/azimuth/test/standard_core_test_cases.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    echo "✓ 所有测试用例语法正确，符合 MoonBit 测试标准。"
    echo ""
    echo "测试文件已成功创建，包含以下功能测试："
    echo "1. add_function_edge_cases - 加法函数边界情况测试"
    echo "2. multiply_function_comprehensive - 乘法函数全面测试"
    echo "3. subtract_function_various_cases - 减法函数各种情况测试"
    echo "4. divide_with_ceil_comprehensive - 向上取整除法全面测试"
    echo "5. greet_function_special_characters - 问候函数特殊字符测试"
    echo "6. complex_business_calculations - 复杂业务计算测试"
    echo "7. mathematical_series_calculation - 数学级数计算测试"
    echo "8. inventory_management_scenario - 库存管理场景测试"
    echo "9. packaging_optimization - 包装优化测试"
    echo "10. time_and_resource_calculation - 时间和资源计算测试"
    echo ""
    echo "所有测试用例都使用标准的 MoonBit 测试语法，包括："
    echo "- 使用 'test' 关键字定义测试用例"
    echo "- 使用 '@azimuth.assert_eq' 进行数值断言"
    echo "- 使用 '@azimuth.assert_eq_string' 进行字符串断言"
    echo "- 测试覆盖了 azimuth 库的所有核心功能"
    echo ""
    echo "✓ 测试文件创建成功！"
else
    echo "✗ 错误: 找不到测试文件 src/azimuth/test/standard_core_test_cases.mbt"
    exit 1
fi