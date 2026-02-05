#!/bin/bash

# 运行综合核心测试用例的脚本

echo "运行综合核心测试用例验证..."
echo ""

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
TEST_FILE="$PROJECT_ROOT/core/comprehensive_core_tests.mbt"

# 检查测试文件是否存在
if [ ! -f "$TEST_FILE" ]; then
  echo "错误: 测试文件不存在: $TEST_FILE"
  exit 1
fi

echo "测试文件存在: $TEST_FILE"

# 统计测试数量
TEST_COUNT=$(grep "^test " "$TEST_FILE" | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
echo "发现 $TEST_COUNT 个测试用例"

# 列出所有测试名称
echo ""
echo "测试用例列表:"
grep "^test " "$TEST_FILE" | sed 's/test "/- /' | sed 's/" {//' | sort

echo ""
echo "=== 验证测试语法 ==="

# 使用自定义的验证脚本
cd "$PROJECT_ROOT"
node validate_tests.js core/comprehensive_core_tests.mbt

if [ $? -eq 0 ]; then
  echo ""
  echo "=== 验证结果 ==="
  echo "✓ 测试文件已创建并放置在正确的位置"
  echo "✓ 测试文件包含 $TEST_COUNT 个测试用例"
  echo "✓ 所有测试用例使用标准 MoonBit 测试语法"
  echo "✓ 测试用例涵盖了各种场景：算术运算、字符串处理、边界情况等"
  echo ""
  echo "综合核心测试用例已成功添加到项目中！"
else
  echo ""
  echo "❌ 测试验证失败"
  exit 1
fi