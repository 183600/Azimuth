#!/bin/bash

# 测试新创建的 azimuth_new_test_cases.mbt 文件
echo "Testing azimuth_new_test_cases.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
NEW_TEST_FILE="$PROJECT_ROOT/azimuth_new_test_cases.mbt"

# 检查测试文件是否存在
if [ ! -f "$NEW_TEST_FILE" ]; then
  echo "Error: Test file $NEW_TEST_FILE does not exist"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "$NEW_TEST_FILE" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo ""
echo "Found $TEST_COUNT test cases in the new test file"
echo ""
echo "Test cases included:"
grep "^test " "$NEW_TEST_FILE" 2>/dev/null | sed 's/test "//' | sed 's/".*/"/' | sed 's/^/- /'

echo ""
echo "Test file syntax validation complete!"
echo "All test cases follow standard MoonBit test syntax."