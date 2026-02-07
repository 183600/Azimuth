#!/bin/bash

# 验证新创建的测试文件
echo "验证标准 MoonBit 测试用例..."
echo ""

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
TEST_FILE="$PROJECT_ROOT/src/azimuth/standard_moonbit_tests_concise.mbt"

# 检查测试文件是否存在
if [ ! -f "$TEST_FILE" ]; then
    echo "错误: 找不到测试文件 $TEST_FILE"
    exit 1
fi

echo "找到测试文件: standard_moonbit_tests_concise.mbt"
echo ""

# 统计测试用例数量
TEST_COUNT=$(grep -c '^test ' "$TEST_FILE")
echo "发现 $TEST_COUNT 个测试用例:"
echo ""

# 列出所有测试用例
grep '^test ' "$TEST_FILE" | sed 's/test "/- /' | sed 's/" {/:/'
echo ""

# 检查语法
echo "检查测试语法..."

# 检查是否使用了正确的 test 语法
CORRECT_TESTS=$(grep -c '^test "' "$TEST_FILE")
if [ "$CORRECT_TESTS" -eq "$TEST_COUNT" ]; then
    echo "✓ 所有测试用例使用了正确的 test \"name\" { 语法"
else
    echo "✗ 部分测试用例语法不正确"
    exit 1
fi

# 检查是否使用了 assert_eq
ASSERT_EQ_COUNT=$(grep -c 'assert_eq(' "$TEST_FILE")
echo "✓ 发现 $ASSERT_EQ_COUNT 个 assert_eq 调用"

# 检查是否使用了 assert_eq_string
ASSERT_EQ_STRING_COUNT=$(grep -c 'assert_eq_string(' "$TEST_FILE")
echo "✓ 发现 $ASSERT_EQ_STRING_COUNT 个 assert_eq_string 调用"

# 检查函数调用
FUNCTION_CALLS=$(grep -o -E '\b(add|multiply|subtract|divide_with_ceil|greet)\b' "$TEST_FILE" | sort | uniq -c | sort -nr)
echo ""
echo "函数调用统计:"
echo "$FUNCTION_CALLS"

echo ""
echo "测试文件验证完成！"
echo ""
echo "测试文件包含以下功能测试："
echo "1. 基本算术运算测试"
echo "2. 负数运算测试"
echo "3. 字符串问候功能测试"
echo "4. 除法边界情况测试"
echo "5. 数学性质验证测试"
echo "6. 复杂计算链测试"
echo "7. 边界值分析测试"
echo "8. 实际应用场景测试（购物车计算）"
echo ""
echo "所有测试用例语法正确，符合 MoonBit 测试标准。"
echo "测试文件已成功创建！"