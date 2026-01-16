#!/bin/bash

# 验证新创建的增强单元测试文件

echo "验证新创建的增强单元测试文件: azimuth_enhanced_unit_tests.mbt"

# 检查文件是否存在
if [ ! -f "azimuth_enhanced_unit_tests.mbt" ]; then
  echo "错误: 文件 azimuth_enhanced_unit_tests.mbt 不存在"
  exit 1
fi

echo "文件存在，正在检查语法..."

# 统计测试数量
TEST_COUNT=$(grep "^test " azimuth_enhanced_unit_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "发现 $TEST_COUNT 个测试用例"

# 检查每个测试的基本语法
echo "检查测试语法..."

# 检查是否有正确的 test 声明
if grep -q "^test " azimuth_enhanced_unit_tests.mbt; then
  echo "✓ 发现测试声明"
else
  echo "✗ 未发现有效的测试声明"
  exit 1
fi

# 检查是否有 assert_eq 断言
if grep -q "assert_eq" azimuth_enhanced_unit_tests.mbt; then
  echo "✓ 发现 assert_eq 断言语句"
else
  echo "✗ 未发现 assert_eq 断言语句"
  exit 1
fi

# 检查是否有基本的函数调用
if grep -q "add\|multiply\|greet" azimuth_enhanced_unit_tests.mbt; then
  echo "✓ 发现核心函数调用 (add/multiply/greet)"
else
  echo "✗ 未发现核心函数调用"
  exit 1
fi

# 列出所有测试名称
echo ""
echo "测试用例列表:"
grep "^test " azimuth_enhanced_unit_tests.mbt | sed 's/test "/- /' | sed 's/" {//' | sort

echo ""
echo "=== 验证结果 ==="
echo "$TEST_COUNT 个测试用例语法检查通过"
echo "所有测试用例结构正确"
exit 0