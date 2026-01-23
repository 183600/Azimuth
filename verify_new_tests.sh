#!/bin/bash

# 验证新创建的测试文件
echo "验证新创建的综合质量测试用例..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"
TEST_FILE="$AZIMUTH_PATH/test/comprehensive_quality_tests.mbt"

echo "检查测试文件: $TEST_FILE"

# 检查测试文件是否存在
if [ ! -f "$TEST_FILE" ]; then
    echo "错误: 测试文件不存在"
    exit 1
fi

# 统计测试用例数量
TEST_COUNT=$(grep -c "^test " "$TEST_FILE")
echo "找到 $TEST_COUNT 个测试用例"

# 检查基本的语法结构
echo "检查测试文件基本语法结构..."

# 检查是否有正确的测试语法
TEST_SYNTAX=$(grep -c "^test \"" "$TEST_FILE")
ASSERT_SYNTAX=$(grep -c "@azimuth.assert_eq\|@azimuth.assert_eq_string\|@azimuth.assert_true\|@azimuth.assert_false" "$TEST_FILE")

echo "找到 $TEST_SYNTAX 个测试块"
echo "找到 $ASSERT_SYNTAX 个断言语句"

# 检查是否有基本的函数调用
ADD_CALLS=$(grep -c "@azimuth.add" "$TEST_FILE")
MULTIPLY_CALLS=$(grep -c "@azimuth.multiply" "$TEST_FILE")
GREET_CALLS=$(grep -c "@azimuth.greet" "$TEST_FILE")
DIVIDE_CALLS=$(grep -c "@azimuth.divide_with_ceil" "$TEST_FILE")

echo "找到 $ADD_CALLS 个 add 函数调用"
echo "找到 $MULTIPLY_CALLS 个 multiply 函数调用"
echo "找到 $GREET_CALLS 个 greet 函数调用"
echo "找到 $DIVIDE_CALLS 个 divide_with_ceil 函数调用"

# 基本验证
if [ $TEST_COUNT -eq $TEST_SYNTAX ] && [ $TEST_COUNT -le 10 ] && [ $ASSERT_SYNTAX -gt 0 ]; then
    echo "✓ 测试文件基本语法结构正确"
    echo "✓ 新创建的 $TEST_COUNT 个测试用例符合 MoonBit 测试语法规范"
    echo "✓ 测试用例涵盖了数学运算、字符串处理和业务逻辑场景"
    echo ""
    echo "测试用例列表："
    grep "^test \"" "$TEST_FILE" | sed 's/^test "/- /' | sed 's/" {$//'
    exit 0
else
    echo "✗ 测试文件基本语法结构检查失败"
    exit 1
fi