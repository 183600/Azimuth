#!/bin/bash

# 真正的测试状态检查脚本
echo "Checking real test compilation status..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/clean_test"

# 编译主包
echo "Compiling main packages..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi
if [ $? -ne 0 ]; then
  echo "Error: azimuth compilation failed"
  exit 1
fi

cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt -o clean_test.mi
if [ $? -ne 0 ]; then
  echo "Error: clean_test compilation failed"
  exit 1
fi

# 检查测试文件
echo ""
echo "Checking test files..."

ERROR_COUNT=0
TOTAL_COUNT=0

# 检查 azimuth 测试
cd "$AZIMUTH_PATH/test"
echo "=== Azimuth test files ==="
for file in *.mbt; do
  if [ -f "$file" ]; then
    TOTAL_COUNT=$((TOTAL_COUNT + 1))
    echo -n "Checking $file... "
    
    node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" >/dev/null 2>&1
    if [ $? -eq 0 ]; then
      echo "OK"
    else
      echo "FAILED"
      ERROR_COUNT=$((ERROR_COUNT + 1))
      # 显示详细错误
      node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" 2>&1 | head -5
    fi
  fi
done

# 检查 clean_test 测试
cd "$CLEAN_TEST_PATH/test"
echo ""
echo "=== Clean_test test files ==="
for file in *.mbt; do
  if [ -f "$file" ]; then
    TOTAL_COUNT=$((TOTAL_COUNT + 1))
    echo -n "Checking $file... "
    
    node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" >/dev/null 2>&1
    if [ $? -eq 0 ]; then
      echo "OK"
    else
      echo "FAILED"
      ERROR_COUNT=$((ERROR_COUNT + 1))
      # 显示详细错误
      node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" 2>&1 | head -5
    fi
  fi
done

echo ""
echo "Summary:"
echo "Total files: $TOTAL_COUNT"
echo "Failed files: $ERROR_COUNT"
echo "Passed files: $((TOTAL_COUNT - ERROR_COUNT))"

if [ $ERROR_COUNT -eq 0 ]; then
  echo "✓ All test files compile successfully"
  exit 0
else
  echo "✗ Some test files have compilation errors"
  exit 1
fi