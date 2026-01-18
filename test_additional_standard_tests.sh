#!/bin/bash

# 测试新添加的标准测试用例
echo "Testing additional_standard_tests.mbt..."

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

# 测试 additional_standard_tests.mbt
echo "Testing additional_standard_tests.mbt..."
cd "$AZIMUTH_PATH/test"

# 编译测试文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi additional_standard_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: additional_standard_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "additional_standard_tests.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in additional_standard_tests.mbt"
echo "All tests compiled successfully!"