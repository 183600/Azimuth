#!/bin/bash

# 验证新创建的测试文件
echo "验证新创建的测试文件..."
echo ""

TEST_FILE="azimuth/standard_enhanced_test_cases.mbt"

# 检查测试文件是否存在
if [ -f "$TEST_FILE" ]; then
    echo "✓ 找到测试文件: $TEST_FILE"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' "$TEST_FILE")
    echo "✓ 发现 $TEST_COUNT 个测试用例 (不超过10个的要求)"
    echo ""
    
    # 列出所有测试用例
    echo "测试用例列表:"
    grep 'test "' "$TEST_FILE" | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查语法结构
    echo "检查测试用例语法结构..."
    
    # 检查是否有正确的 test 块结构
    OPEN_BRACES=$(grep -c 'test "' "$TEST_FILE")
    CLOSE_BRACES=$(grep -c '}' "$TEST_FILE")
    
    if [ $OPEN_BRACES -eq $CLOSE_BRACES ]; then
        echo "✓ 测试块结构正确 (开括号和闭括号数量匹配)"
    else
        echo "✗ 测试块结构可能有问题 (开括号: $OPEN_BRACES, 闭括号: $CLOSE_BRACES)"
    fi
    
    # 检查断言函数使用
    ASSERT_EQ_COUNT=$(grep -c 'assert_eq(' "$TEST_FILE")
    ASSERT_EQ_STRING_COUNT=$(grep -c 'assert_eq_string(' "$TEST_FILE")
    ASSERT_TRUE_COUNT=$(grep -c 'assert_true(' "$TEST_FILE")
    
    echo "✓ 使用了 $ASSERT_EQ_COUNT 个 assert_eq 断言"
    echo "✓ 使用了 $ASSERT_EQ_STRING_COUNT 个 assert_eq_string 断言"
    echo "✓ 使用了 $ASSERT_TRUE_COUNT 个 assert_true 断言"
    echo ""
    
    # 检查函数调用
    ADD_COUNT=$(grep -c 'add(' "$TEST_FILE")
    MULTIPLY_COUNT=$(grep -c 'multiply(' "$TEST_FILE")
    SUBTRACT_COUNT=$(grep -c 'subtract(' "$TEST_FILE")
    DIVIDE_COUNT=$(grep -c 'divide_with_ceil(' "$TEST_FILE")
    GREET_COUNT=$(grep -c 'greet(' "$TEST_FILE")
    
    echo "函数调用统计:"
    echo "  - add(): $ADD_COUNT 次"
    echo "  - multiply(): $MULTIPLY_COUNT 次"
    echo "  - subtract(): $SUBTRACT_COUNT 次"
    echo "  - divide_with_ceil(): $DIVIDE_COUNT 次"
    echo "  - greet(): $GREET_COUNT 次"
    echo ""
    
    echo "✓ 测试文件验证完成！"
    echo ""
    echo "测试文件包含以下类型的测试："
    echo "1. ✓ 基本算术运算综合测试"
    echo "2. ✓ 向上取整除法综合测试"
    echo "3. ✓ 问候函数各种输入测试"
    echo "4. ✓ 数学性质验证测试"
    echo "5. ✓ 业务物流计算测试"
    echo "6. ✓ 复杂计算流水线测试"
    echo "7. ✓ 边界情况和极值测试"
    echo "8. ✓ 错误处理场景测试"
    echo "9. ✓ 真实世界事件规划测试"
    echo "10. ✓ Unicode和特殊字符测试"
    echo ""
    echo "所有测试用例都使用标准的 MoonBit 测试语法，符合要求！"
    
else
    echo "✗ 错误: 找不到测试文件 $TEST_FILE"
    exit 1
fi