#!/bin/bash

# 验证 azimuth_standard_tests.mbt 文件
echo "验证 azimuth_standard_tests.mbt 文件..."

TEST_FILE="/home/runner/work/Azimuth/Azimuth/azimuth/test/azimuth_standard_tests.mbt"

if [ -f "$TEST_FILE" ]; then
  echo "✓ 测试文件存在: $TEST_FILE"
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " "$TEST_FILE" | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  
  echo "✓ 测试用例数量: $TEST_COUNT"
  
  # 显示测试名称列表
  echo ""
  echo "测试用例列表:"
  grep "^test " "$TEST_FILE" | sed 's/test "//g' | sed 's/" {//g' | nl
  
  # 检查是否包含10个测试用例
  if [ "$TEST_COUNT" -eq 10 ]; then
    echo ""
    echo "✓ 成功创建了10个标准MoonBit测试用例"
    exit 0
  else
    echo ""
    echo "✗ 测试用例数量不正确，期望10个，实际$TEST_COUNT个"
    exit 1
  fi
else
  echo "✗ 测试文件不存在: $TEST_FILE"
  exit 1
fi