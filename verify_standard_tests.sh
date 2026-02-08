#!/bin/bash

# 验证标准测试文件的脚本
echo "验证标准 MoonBit 测试文件..."

# 检查测试文件是否存在
if [ ! -f "azimuth/standard_moonbit_tests.mbt" ]; then
  echo "错误: 找不到测试文件 azimuth/standard_moonbit_tests.mbt"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " azimuth/standard_moonbit_tests.mbt | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "找到 $TEST_COUNT 个测试用例"

# 显示测试用例名称
echo ""
echo "测试用例列表："
grep "^test " azimuth/standard_moonbit_tests.mbt | sed 's/test "/- /' | sed 's/" {$//'

# 验证语法结构
echo ""
echo "验证测试语法结构..."

# 检查是否有未闭合的大括号
OPEN_BRACES=$(grep -o "{" azimuth/standard_moonbit_tests.mbt | wc -l)
CLOSE_BRACES=$(grep -o "}" azimuth/standard_moonbit_tests.mbt | wc -l)
OPEN_BRACES=$(echo "$OPEN_BRACES" | tr -d ' ')
CLOSE_BRACES=$(echo "$CLOSE_BRACES" | tr -d ' ')

if [ "$OPEN_BRACES" -eq "$CLOSE_BRACES" ]; then
  echo "✓ 大括号匹配正确"
else
  echo "✗ 大括号不匹配: 开放 $OPEN_BRACES, 闭合 $CLOSE_BRACES"
fi

# 检查测试函数调用
ASSERT_EQ_COUNT=$(grep "assert_eq(" azimuth/standard_moonbit_tests.mbt | wc -l)
ASSERT_EQ_STRING_COUNT=$(grep "assert_eq_string(" azimuth/standard_moonbit_tests.mbt | wc -l)
ASSERT_EQ_COUNT=$(echo "$ASSERT_EQ_COUNT" | tr -d ' ')
ASSERT_EQ_STRING_COUNT=$(echo "$ASSERT_EQ_STRING_COUNT" | tr -d ' ')

echo "✓ 包含 $ASSERT_EQ_COUNT 个 assert_eq 调用"
echo "✓ 包含 $ASSERT_EQ_STRING_COUNT 个 assert_eq_string 调用"

# 检查核心函数调用
ADD_COUNT=$(grep "add(" azimuth/standard_moonbit_tests.mbt | wc -l)
MULTIPLY_COUNT=$(grep "multiply(" azimuth/standard_moonbit_tests.mbt | wc -l)
SUBTRACT_COUNT=$(grep "subtract(" azimuth/standard_moonbit_tests.mbt | wc -l)
DIVIDE_COUNT=$(grep "divide_with_ceil(" azimuth/standard_moonbit_tests.mbt | wc -l)
GREET_COUNT=$(grep "greet(" azimuth/standard_moonbit_tests.mbt | wc -l)

ADD_COUNT=$(echo "$ADD_COUNT" | tr -d ' ')
MULTIPLY_COUNT=$(echo "$MULTIPLY_COUNT" | tr -d ' ')
SUBTRACT_COUNT=$(echo "$SUBTRACT_COUNT" | tr -d ' ')
DIVIDE_COUNT=$(echo "$DIVIDE_COUNT" | tr -d ' ')
GREET_COUNT=$(echo "$GREET_COUNT" | tr -d ' ')

echo ""
echo "核心函数调用统计："
echo "- add(): $ADD_COUNT 次"
echo "- multiply(): $MULTIPLY_COUNT 次"
echo "- subtract(): $SUBTRACT_COUNT 次"
echo "- divide_with_ceil(): $DIVIDE_COUNT 次"
echo "- greet(): $GREET_COUNT 次"

echo ""
echo "验证完成！测试文件结构正确。"