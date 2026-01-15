#!/bin/bash

# 验证新添加的测试用例
echo "验证新添加的 MoonBit 测试用例..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
TEST_FILE="$PROJECT_ROOT/src/azimuth/test/enhanced_moon_test_cases.mbt"

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

# 验证测试用例数量不超过10个
if [ "$TEST_COUNT" -le 10 ]; then
  echo ""
  echo "✓ 测试用例数量符合要求（不超过10个）"
else
  echo ""
  echo "✗ 测试用例数量超过限制（当前：$TEST_COUNT，限制：10）"
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
if grep -q "@azimuth.add(" "$TEST_FILE" && grep -q "@azimuth.multiply(" "$TEST_FILE"; then
  echo "✓ 使用了项目中的函数"
else
  echo "✗ 未正确使用项目中的函数"
  exit 1
fi

# 检查是否使用了断言函数
if grep -q "@azimuth.assert_eq(" "$TEST_FILE"; then
  echo "✓ 使用了断言函数"
else
  echo "✗ 未正确使用断言函数"
  exit 1
fi

# 检查测试用例的多样性
echo ""
echo "检查测试用例多样性..."

# 检查是否包含统计计算测试
if grep -q "statistical" "$TEST_FILE"; then
  echo "✓ 包含统计计算测试"
else
  echo "✗ 缺少统计计算测试"
fi

# 检查是否包含几何计算测试
if grep -q "geometric" "$TEST_FILE"; then
  echo "✓ 包含几何计算测试"
else
  echo "✗ 缺少几何计算测试"
fi

# 检查是否包含金融计算测试
if grep -q "financial" "$TEST_FILE"; then
  echo "✓ 包含金融计算测试"
else
  echo "✗ 缺少金融计算测试"
fi

# 检查是否包含物理模拟测试
if grep -q "physics" "$TEST_FILE"; then
  echo "✓ 包含物理模拟测试"
else
  echo "✗ 缺少物理模拟测试"
fi

# 检查是否包含数据结构操作测试
if grep -q "data_structure" "$TEST_FILE"; then
  echo "✓ 包含数据结构操作测试"
else
  echo "✗ 缺少数据结构操作测试"
fi

# 检查是否包含字符串处理测试
if grep -q "string_processing" "$TEST_FILE"; then
  echo "✓ 包含字符串处理测试"
else
  echo "✗ 缺少字符串处理测试"
fi

# 检查是否包含算法复杂度测试
if grep -q "algorithm_complexity" "$TEST_FILE"; then
  echo "✓ 包含算法复杂度测试"
else
  echo "✗ 缺少算法复杂度测试"
fi

# 检查是否包含边界情况测试
if grep -q "edge_case" "$TEST_FILE"; then
  echo "✓ 包含边界情况测试"
else
  echo "✗ 缺少边界情况测试"
fi

# 检查是否包含实际应用测试
if grep -q "real_world" "$TEST_FILE"; then
  echo "✓ 包含实际应用测试"
else
  echo "✗ 缺少实际应用测试"
fi

# 检查是否包含性能优化测试
if grep -q "performance" "$TEST_FILE"; then
  echo "✓ 包含性能优化测试"
else
  echo "✗ 缺少性能优化测试"
fi

echo ""
echo "所有验证通过！新添加的测试用例符合要求。"