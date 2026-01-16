#!/bin/bash

# 测试新创建的 MoonBit 测试用例
echo "Testing new MoonBit test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
NEW_TEST_FILE="$PROJECT_ROOT/azimuth_new_standard_tests.mbt"

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
if [ $? -ne 0 ]; then
  echo "Error: azimuth.mi generation failed"
  exit 1
fi

# 编译新的测试文件
echo "Compiling new test file..."
cd "$PROJECT_ROOT"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i "$AZIMUTH_PATH/azimuth.mi" "$NEW_TEST_FILE"
if [ $? -ne 0 ]; then
  echo "Error: New test file compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "$NEW_TEST_FILE" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo ""
echo "Test compilation successful!"
echo "Found $TEST_COUNT test cases in the new test file"
echo ""
echo "Test cases included:"
grep "^test " "$NEW_TEST_FILE" 2>/dev/null | sed 's/test "//' | sed 's/".*/"/' | sed 's/^/- /'

echo ""
echo "All test cases compiled successfully!"