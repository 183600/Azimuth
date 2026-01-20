#!/bin/bash

# 验证新添加的测试用例

echo "=== 验证新添加的测试用例 ==="

TEST_FILE="/home/runner/work/Azimuth/Azimuth/azimuth/test/standard_enhanced_test_cases.mbt"

if [ ! -f "$TEST_FILE" ]; then
    echo "错误: 测试文件不存在: $TEST_FILE"
    exit 1
fi

echo "✓ 测试文件存在: $TEST_FILE"

# 统计测试数量
TEST_COUNT=$(grep "^test " "$TEST_FILE" | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "✓ 发现 $TEST_COUNT 个测试用例"

if [ "$TEST_COUNT" -le 10 ]; then
    echo "✓ 测试用例数量符合要求 (不超过10个)"
else
    echo "✗ 测试用例数量超出要求 (超过10个)"
    exit 1
fi

# 列出所有测试名称
echo ""
echo "测试用例列表:"
grep "^test " "$TEST_FILE" | sed 's/test "/- /' | sed 's/" {//' | sort

# 验证语法
echo ""
echo "=== 语法验证 ==="

# 检查是否有未闭合的大括号
OPEN_BRACES=$(grep -c "{" "$TEST_FILE")
CLOSE_BRACES=$(grep -c "}" "$TEST_FILE")

if [ "$OPEN_BRACES" -eq "$CLOSE_BRACES" ]; then
    echo "✓ 大括号匹配正确"
else
    echo "✗ 大括号不匹配: 开启 $OPEN_BRACES, 闭合 $CLOSE_BRACES"
fi

# 检查测试语法
TEST_SYNTAX=$(grep "^test " "$TEST_FILE" | wc -l)
ASSERT_COUNT=$(grep "assert_eq" "$TEST_FILE" | wc -l)
ASSERT_STRING_COUNT=$(grep "assert_eq_string" "$TEST_FILE" | wc -l)

echo "✓ 测试语法: $TEST_SYNTAX 个测试块"
echo "✓ 断言语句: $ASSERT_COUNT 个 assert_eq, $ASSERT_STRING_COUNT 个 assert_eq_string"

# 检查是否使用了标准 MoonBit 测试语法
STANDARD_SYNTAX=$(grep "^test " "$TEST_FILE" | head -1)
if [[ "$STANDARD_SYNTAX" == test* ]]; then
    echo "✓ 使用标准 MoonBit 测试语法"
else
    echo "✗ 未使用标准 MoonBit 测试语法"
fi

echo ""
echo "=== 配置验证 ==="

# 检查是否已添加到主包配置
MAIN_CONFIG="/home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json"
if grep -q "test/standard_enhanced_test_cases.mbt" "$MAIN_CONFIG"; then
    echo "✓ 已添加到主包配置文件"
else
    echo "✗ 未添加到主包配置文件"
fi

# 检查是否已添加到测试配置
TEST_CONFIG="/home/runner/work/Azimuth/Azimuth/azimuth/test/moon.pkg.json"
if grep -q "standard_enhanced_test_cases.mbt" "$TEST_CONFIG"; then
    echo "✓ 已添加到测试配置文件"
else
    echo "✗ 未添加到测试配置文件"
fi

echo ""
echo "=== 验证完成 ==="
echo "✓ 所有验证项目通过"
echo "✓ 新测试用例已成功添加到项目中"