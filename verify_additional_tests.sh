#!/bin/bash

# 验证添加的测试用例

echo "验证新添加的测试用例..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/clean_test"

# 检查测试文件是否存在
TEST_FILE="$CLEAN_TEST_PATH/test/additional_tests.mbt"
if [ -f "$TEST_FILE" ]; then
  echo "测试文件已创建: $TEST_FILE"
  
  # 统计测试用例数量
  TEST_COUNT=$(grep -c "^test " "$TEST_FILE")
  echo "测试用例数量: $TEST_COUNT"
  
  # 列出所有测试用例
  echo "测试用例列表:"
  grep "^test " "$TEST_FILE"
  
  # 验证语法
  echo "验证测试语法..."
  cd "$CLEAN_TEST_PATH"
  node moonc.js check -pkg test test/additional_tests.mbt
  if [ $? -eq 0 ]; then
    echo "测试语法验证成功！"
  else
    echo "测试语法验证失败！"
  fi
else
  echo "错误: 测试文件不存在！"
fi