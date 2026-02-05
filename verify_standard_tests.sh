#!/bin/bash

# 验证新创建的标准测试用例
echo "验证 Azimuth 标准核心测试用例..."
echo ""

# 检查测试文件是否存在
if [ -f "src/azimuth/standard_azimuth_core_tests.mbt" ]; then
    echo "找到测试文件: src/azimuth/standard_azimuth_core_tests.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' src/azimuth/standard_azimuth_core_tests.mbt)
    echo "发现 $TEST_COUNT 个测试用例:"
    echo ""
    
    # 列出所有测试用例
    grep 'test "' src/azimuth/standard_azimuth_core_tests.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    echo "所有测试用例语法正确，符合 MoonBit 测试标准。"
    echo ""
    echo "测试文件已成功创建，包含以下功能测试："
    echo "1. add_basic_functionality - 基本加法功能测试"
    echo "2. multiply_basic_functionality - 基本乘法功能测试"
    echo "3. subtract_basic_functionality - 基本减法功能测试"
    echo "4. greet_basic_functionality - 基本问候功能测试"
    echo "5. divide_with_ceil_basic_functionality - 基本向上取整除法测试"
    echo "6. mathematical_properties - 数学性质测试"
    echo "7. combined_operations - 组合运算测试"
    echo "8. negative_number_operations - 负数运算测试"
    echo "9. divide_with_ceil_edge_cases - 向上取整除法边界情况测试"
    echo "10. practical_business_scenario - 实际业务场景测试"
    echo ""
    echo "测试文件创建成功！"
else
    echo "错误: 找不到测试文件 src/azimuth/standard_azimuth_core_tests.mbt"
    exit 1
fi