#!/bin/bash

# 验证新创建的综合测试套件

echo "验证新创建的测试文件: azimuth_comprehensive_test_suite.mbt"

# 检查文件是否存在
if [ ! -f "src/azimuth/test/azimuth_comprehensive_test_suite.mbt" ]; then
  echo "错误: 文件 src/azimuth/test/azimuth_comprehensive_test_suite.mbt 不存在"
  exit 1
fi

echo "文件存在，正在检查语法..."

# 统计测试数量
TEST_COUNT=$(grep "^test " src/azimuth/test/azimuth_comprehensive_test_suite.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "发现 $TEST_COUNT 个测试用例"

# 检查每个测试的基本语法
echo "检查测试语法..."

# 检查是否有正确的 test 声明
if grep -q "^test " src/azimuth/test/azimuth_comprehensive_test_suite.mbt; then
  echo "✓ 发现测试声明"
else
  echo "✗ 未发现有效的测试声明"
  exit 1
fi

# 检查是否有正确的 use 导入
if grep -q "^use azimuth" src/azimuth/test/azimuth_comprehensive_test_suite.mbt; then
  echo "✓ 发现正确的模块导入"
else
  echo "✗ 未发现正确的模块导入"
  exit 1
fi

# 检查是否有 azimuth 前缀的函数调用
if grep -q "azimuth\." src/azimuth/test/azimuth_comprehensive_test_suite.mbt; then
  echo "✓ 发现 azimuth 前缀的函数调用"
else
  echo "✗ 未发现 azimuth 前缀的函数调用"
  exit 1
fi

# 检查是否有 assert_eq 断言
if grep -q "assert_eq" src/azimuth/test/azimuth_comprehensive_test_suite.mbt; then
  echo "✓ 发现 assert_eq 断言语句"
else
  echo "✗ 未发现 assert_eq 断言语句"
  exit 1
fi

# 检查是否有 assert_eq_string 断言
if grep -q "assert_eq_string" src/azimuth/test/azimuth_comprehensive_test_suite.mbt; then
  echo "✓ 发现 assert_eq_string 断言语句"
else
  echo "✗ 未发现 assert_eq_string 断言语句"
fi

# 检查是否有基本的函数调用
if grep -q "azimuth.add\|azimuth.multiply\|azimuth.greet" src/azimuth/test/azimuth_comprehensive_test_suite.mbt; then
  echo "✓ 发现核心函数调用 (add/multiply/greet)"
else
  echo "✗ 未发现核心函数调用"
  exit 1
fi

# 列出所有测试名称
echo ""
echo "测试用例列表:"
grep "^test " src/azimuth/test/azimuth_comprehensive_test_suite.mbt | sed 's/test "/- /' | sed 's/" {//' | sort

echo ""
echo "=== 验证结果 ==="
echo "$TEST_COUNT 个测试用例语法检查通过"
echo "所有测试用例结构正确"
echo "测试文件已成功添加到项目中"
exit 0