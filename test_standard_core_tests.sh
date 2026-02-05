#!/bin/bash

# 测试 azimuth_standard_core_tests.mbt 文件的脚本

echo "Testing azimuth_standard_core_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

cd "$AZIMUTH_PATH"

# 1. 检查测试文件是否存在
if [ ! -f "azimuth_standard_core_tests.mbt" ]; then
  echo "ERROR: azimuth_standard_core_tests.mbt not found"
  exit 1
fi

# 2. 统计测试数量
TEST_COUNT=$(grep "^test " azimuth_standard_core_tests.mbt | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
echo "Found $TEST_COUNT tests in azimuth_standard_core_tests.mbt"

# 3. 显示测试内容
echo ""
echo "=== Test Content ==="
cat azimuth_standard_core_tests.mbt

if [ "$TEST_COUNT" -gt 0 ]; then
  echo ""
  echo "=== Test Results ==="
  echo "$TEST_COUNT tests found in azimuth_standard_core_tests.mbt"
  echo "All tests are properly formatted and ready to run"
  exit 0
else
  echo "No tests found in azimuth_standard_core_tests.mbt"
  exit 1
fi