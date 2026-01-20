#!/bin/bash

# 检查新创建的测试文件
echo "Checking new test file..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 检查新创建的测试文件
echo "Checking new test file: standard_comprehensive_test_suite.mbt"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i azimuth.mi standard_comprehensive_test_suite.mbt
if [ $? -eq 0 ]; then
  echo "✓ 新测试文件编译成功!"
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " standard_comprehensive_test_suite.mbt | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  echo "✓ 包含 $TEST_COUNT 个测试用例"
  
  # 列出所有测试用例
  echo ""
  echo "测试用例列表:"
  grep "^test " standard_comprehensive_test_suite.mbt
else
  echo "✗ 新测试文件编译失败"
  exit 1
fi