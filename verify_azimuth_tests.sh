#!/bin/bash

# 验证 azimuth_test.mbt 中新添加的测试用例
echo "验证 azimuth_test.mbt 中新添加的 MoonBit 测试用例..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
TEST_FILE="$PROJECT_ROOT/src/azimuth/test/azimuth_test.mbt"

# 检查测试文件是否存在
if [ ! -f "$TEST_FILE" ]; then
  echo "错误：测试文件不存在"
  exit 1
fi

# 统计测试用例数量
TEST_COUNT=$(grep "^test " "$TEST_FILE" | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "找到 $TEST_COUNT 个测试用例"

# 列出所有测试用例
echo ""
echo "测试用例列表："
grep "^test " "$TEST_FILE" | sed 's/test "/- /' | sed 's/" {/ /'

# 检查是否有新添加的测试用例
NEW_TESTS_COUNT=$(grep "^test \"large_number_operations\|^test \"negative_number_operations\|^test \"boundary_value_operations\|^test \"string_processing_extreme\|^test \"mathematical_properties_validation\|^test \"real_world_currency_conversion\|^test \"error_boundary_handling\|^test \"performance_calculation_patterns\|^test \"complex_formula_evaluation\|^test \"data_type_boundary_simulation" "$TEST_FILE" | wc -l)
NEW_TESTS_COUNT=$(echo "$NEW_TESTS_COUNT" | tr -d ' ')

echo ""
echo "新添加的测试用例数量：$NEW_TESTS_COUNT"

# 验证新添加的测试用例数量为10个
if [ "$NEW_TESTS_COUNT" -eq 10 ]; then
  echo "✓ 新添加的测试用例数量符合要求（10个）"
else
  echo "✗ 新添加的测试用例数量不符合要求（当前：$NEW_TESTS_COUNT，要求：10）"
  exit 1
fi

# 检查测试语法
echo ""
echo "检查测试语法..."

# 检查是否有未闭合的大括号
OPEN_BRACES=$(grep -c "{" "$TEST_FILE")
CLOSE_BRACES=$(grep -c "}" "$TEST_FILE")

if [ "$OPEN_BRACES" -eq "$CLOSE_BRACES" ]; then
  echo "✓ 大括号匹配正确"
else
  echo "✗ 大括号不匹配（开放：$OPEN_BRACES，闭合：$CLOSE_BRACES）"
  exit 1
fi

# 检查是否使用了标准的测试语法
if grep -q "^test " "$TEST_FILE"; then
  echo "✓ 使用了标准的 test 语法"
else
  echo "✗ 未找到标准的 test 语法"
  exit 1
fi

# 检查是否使用了项目中的函数
if grep -q "@azimuth.add(" "$TEST_FILE" && grep -q "@azimuth.multiply(" "$TEST_FILE" && grep -q "@azimuth.greet(" "$TEST_FILE"; then
  echo "✓ 使用了项目中的函数"
else
  echo "✗ 未正确使用项目中的函数"
  exit 1
fi

# 检查是否使用了断言函数
if grep -q "@azimuth.assert_eq(" "$TEST_FILE" && grep -q "@azimuth.assert_eq_string(" "$TEST_FILE"; then
  echo "✓ 使用了断言函数"
else
  echo "✗ 未正确使用断言函数"
  exit 1
fi

# 检查新添加的测试用例的具体内容
echo ""
echo "检查新添加的测试用例内容..."

# 检查大数运算测试
if grep -q "test \"large_number_operations\"" "$TEST_FILE"; then
  echo "✓ 包含大数运算测试"
else
  echo "✗ 缺少大数运算测试"
  exit 1
fi

# 检查负数运算测试
if grep -q "test \"negative_number_operations\"" "$TEST_FILE"; then
  echo "✓ 包含负数运算测试"
else
  echo "✗ 缺少负数运算测试"
  exit 1
fi

# 检查边界值测试
if grep -q "test \"boundary_value_operations\"" "$TEST_FILE"; then
  echo "✓ 包含边界值测试"
else
  echo "✗ 缺少边界值测试"
  exit 1
fi

# 检查字符串处理测试
if grep -q "test \"string_processing_extreme\"" "$TEST_FILE"; then
  echo "✓ 包含字符串处理测试"
else
  echo "✗ 缺少字符串处理测试"
  exit 1
fi

# 检查数学性质验证测试
if grep -q "test \"mathematical_properties_validation\"" "$TEST_FILE"; then
  echo "✓ 包含数学性质验证测试"
else
  echo "✗ 缺少数学性质验证测试"
  exit 1
fi

# 检查实际应用测试
if grep -q "test \"real_world_currency_conversion\"" "$TEST_FILE"; then
  echo "✓ 包含实际应用测试"
else
  echo "✗ 缺少实际应用测试"
  exit 1
fi

# 检查错误处理测试
if grep -q "test \"error_boundary_handling\"" "$TEST_FILE"; then
  echo "✓ 包含错误处理测试"
else
  echo "✗ 缺少错误处理测试"
  exit 1
fi

# 检查性能计算测试
if grep -q "test \"performance_calculation_patterns\"" "$TEST_FILE"; then
  echo "✓ 包含性能计算测试"
else
  echo "✗ 缺少性能计算测试"
  exit 1
fi

# 检查复杂公式测试
if grep -q "test \"complex_formula_evaluation\"" "$TEST_FILE"; then
  echo "✓ 包含复杂公式测试"
else
  echo "✗ 缺少复杂公式测试"
  exit 1
fi

# 检查数据类型边界测试
if grep -q "test \"data_type_boundary_simulation\"" "$TEST_FILE"; then
  echo "✓ 包含数据类型边界测试"
else
  echo "✗ 缺少数据类型边界测试"
  exit 1
fi

echo ""
echo "所有验证通过！新添加的10个测试用例符合要求。"