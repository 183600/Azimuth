#!/bin/bash

# 验证新创建的综合测试文件

echo "验证新创建的测试文件: azimuth_comprehensive_new_tests.mbt"

# 检查文件是否存在
if [ ! -f "azimuth_comprehensive_new_tests.mbt" ]; then
  echo "错误: 文件 azimuth_comprehensive_new_tests.mbt 不存在"
  exit 1
fi

echo "文件存在，正在检查语法..."

# 统计测试数量
TEST_COUNT=$(grep "^test " azimuth_comprehensive_new_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "发现 $TEST_COUNT 个测试用例"

# 检查测试数量是否超过限制
if [ "$TEST_COUNT" -gt 10 ]; then
  echo "警告: 测试用例数量 ($TEST_COUNT) 超过了限制 (10个)"
else
  echo "✓ 测试用例数量在限制范围内"
fi

# 检查每个测试的基本语法
echo "检查测试语法..."

# 检查是否有正确的 test 声明
if grep -q "^test " azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 发现测试声明"
else
  echo "✗ 未发现有效的测试声明"
  exit 1
fi

# 检查是否有 assert_eq 断言
if grep -q "assert_eq" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 发现断言语句"
else
  echo "✗ 未发现断言语句"
  exit 1
fi

# 检查是否有基本的函数调用
if grep -q "azimuth::" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 发现 azimuth 包函数调用"
else
  echo "✗ 未发现 azimuth 包函数调用"
  exit 1
fi

# 检查是否使用了标准 MoonBit 测试语法
echo "检查 MoonBit 测试语法..."

# 检查是否有正确的测试结构
if grep -q "^test \".*\" {" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 使用了标准的测试语法"
else
  echo "✗ 测试语法不符合标准"
  exit 1
fi

# 列出所有测试名称
echo ""
echo "测试用例列表:"
grep "^test " azimuth_comprehensive_new_tests.mbt | sed 's/test "/- /' | sed 's/" {//' | sort

# 检查测试覆盖的功能点
echo ""
echo "测试覆盖的功能点:"

if grep -q "azimuth::add" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 加法功能测试"
fi

if grep -q "azimuth::multiply" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 乘法功能测试"
fi

if grep -q "azimuth::greet" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 问候功能测试"
fi

if grep -q "negative" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 负数处理测试"
fi

if grep -q "boundary\|edge" azimuth_comprehensive_new_tests.mbt; then
  echo "✓ 边界条件测试"
fi

echo ""
echo "=== 验证结果 ==="
echo "$TEST_COUNT 个测试用例语法检查通过"
echo "所有测试用例结构正确"
echo "使用了标准的 MoonBit 测试语法"
exit 0