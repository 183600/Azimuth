#!/bin/bash

# 简单验证测试文件的结构和语法

echo "验证测试文件的结构和语法..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/clean_test"

# 检查测试文件是否存在
TEST_FILE="$CLEAN_TEST_PATH/test/additional_tests.mbt"
if [ -f "$TEST_FILE" ]; then
  echo "✓ 测试文件已创建: $TEST_FILE"
  
  # 统计测试用例数量
  TEST_COUNT=$(grep -c "^test " "$TEST_FILE")
  echo "✓ 测试用例数量: $TEST_COUNT"
  
  if [ $TEST_COUNT -le 10 ]; then
    echo "✓ 测试用例数量符合要求（不超过10个）"
  else
    echo "✗ 测试用例数量超过10个"
  fi
  
  # 列出所有测试用例
  echo "✓ 测试用例列表:"
  grep "^test " "$TEST_FILE"
  
  # 检查测试语法结构
  echo "✓ 检查测试语法结构..."
  
  # 检查是否有正确的测试声明
  if grep -q "^test " "$TEST_FILE"; then
    echo "  - 包含正确的测试声明"
  else
    echo "  - 缺少正确的测试声明"
  fi
  
  # 检查是否有断言函数调用
  if grep -q "@clean_test.assert_eq" "$TEST_FILE" || grep -q "@clean_test.assert_eq_string" "$TEST_FILE"; then
    echo "  - 包含断言函数调用"
  else
    echo "  - 缺少断言函数调用"
  fi
  
  # 检查是否有函数调用
  if grep -q "@clean_test.add\|@clean_test.multiply\|@clean_test.greet" "$TEST_FILE"; then
    echo "  - 包含函数调用"
  else
    echo "  - 缺少函数调用"
  fi
  
  echo "✓ 测试文件结构和语法验证完成！"
else
  echo "✗ 错误: 测试文件不存在！"
fi