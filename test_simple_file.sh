#!/bin/bash

# 测试最简单的测试文件
echo "Testing simple_test.mbt file..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

cd "$AZIMUTH_PATH"

# 1. 编译主包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth" -std-path "$PROJECT_ROOT/core" -o "azimuth.mi" lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth package compilation failed"
  exit 1
fi

# 2. 编译简单测试文件
echo "Compiling simple_test.mbt file..."
cd test

node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth_test" -std-path "$PROJECT_ROOT/core" -i "../azimuth.mi" simple_test.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: simple_test.mbt file compilation failed"
  exit 1
fi

# 3. 统计测试数量
TEST_COUNT=$(grep "^test " simple_test.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in simple_test.mbt"
echo "All tests compiled successfully!"

exit 0