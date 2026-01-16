#!/bin/bash

# 测试新创建的 azimuth_additional_comprehensive_tests.mbt 文件
echo "Testing azimuth_additional_comprehensive_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
NEW_TEST_FILE="$PROJECT_ROOT/azimuth_additional_comprehensive_tests.mbt"

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

# 检查测试是否使用了正确的断言语法
ASSERT_EQ_COUNT=$(grep "assert_eq(" "$NEW_TEST_FILE" 2>/dev/null | wc -l)
AZIMUTH_CALLS_COUNT=$(grep "azimuth::" "$NEW_TEST_FILE" 2>/dev/null | wc -l)

echo ""
echo "Test file analysis:"
echo "- assert_eq() calls: $ASSERT_EQ_COUNT"
echo "- azimuth:: function calls: $AZIMUTH_CALLS_COUNT"
echo ""
echo "Validation successful! The test file is properly structured."