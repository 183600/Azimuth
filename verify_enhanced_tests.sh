#!/bin/bash

# 验证增强测试套件的脚本

echo "验证增强测试套件..."

# 编译测试文件
echo "编译测试文件..."
node moonc.js check -pkg azimuth_test -std-path core azimuth_enhanced_test_suite.mbt

if [ $? -eq 0 ]; then
  echo "测试文件编译成功！"
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " azimuth_enhanced_test_suite.mbt | wc -l)
  echo "发现 $TEST_COUNT 个测试用例"
  
  echo ""
  echo "测试用例列表："
  grep "^test " azimuth_enhanced_test_suite.mbt | sed 's/test "\(.*\)" {/  - \1/'
  
  echo ""
  echo "所有测试已成功添加并验证！"
else
  echo "测试文件编译失败！"
  exit 1
fi