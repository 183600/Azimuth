#!/bin/bash

# 测试新创建的标准测试用例 - 增强版
echo "Testing azimuth_standard_tests_enhanced.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
TEST_PATH="$AZIMUTH_PATH/test"

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

# 测试 azimuth_standard_tests_enhanced.mbt
echo "Testing azimuth_standard_tests_enhanced.mbt..."
cd "$TEST_PATH"

# 编译测试文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi azimuth_standard_tests_enhanced.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_standard_tests_enhanced.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "azimuth_standard_tests_enhanced.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in azimuth_standard_tests_enhanced.mbt"
echo "All tests compiled successfully!"