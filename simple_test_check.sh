#!/bin/bash

# 简单的测试运行脚本
echo "Running simple test check..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi
if [ $? -ne 0 ]; then
  echo "Error: azimuth compilation failed"
  exit 1
fi

# 编译 clean_test 包
echo "Compiling clean_test..."
cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt -o clean_test.mi
if [ $? -ne 0 ]; then
  echo "Error: clean_test compilation failed"
  exit 1
fi

# 检查测试文件编译
echo ""
echo "Checking test file compilation..."

ERROR_COUNT=0
TOTAL_FILES=0

# 检查 azimuth 测试
cd "$AZIMUTH_PATH/test"
for file in *.mbt; do
  if [ -f "$file" ]; then
    TOTAL_FILES=$((TOTAL_FILES + 1))
    echo "Checking $file..."
    node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" 2>&1 | grep -E "\[E[0-9]+\]" | grep -v "Warning" && ERROR_COUNT=$((ERROR_COUNT + 1))
  fi
done

# 检查 clean_test 测试
cd "$CLEAN_TEST_PATH/test"
for file in *.mbt; do
  if [ -f "$file" ]; then
    TOTAL_FILES=$((TOTAL_FILES + 1))
    echo "Checking $file..."
    node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" 2>&1 | grep -E "\[E[0-9]+\]" | grep -v "Warning" && ERROR_COUNT=$((ERROR_COUNT + 1))
  fi
done

echo ""
echo "Checked $TOTAL_FILES test files"
if [ $ERROR_COUNT -eq 0 ]; then
  echo "✓ No compilation errors found"
  exit 0
else
  echo "✗ Found $ERROR_COUNT files with errors"
  exit 1
fi