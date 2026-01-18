#!/bin/bash

# 测试新创建的简化标准测试用例
echo "Testing azimuth_simple_standard_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"

# 测试 azimuth_simple_standard_tests.mbt
echo "Testing azimuth_simple_standard_tests.mbt..."
cd "$PROJECT_ROOT"

# 编译测试文件
node "$PROJECT_ROOT/moonc.js" check -pkg simple_test -std-path "$CORE_PATH" azimuth_simple_standard_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_simple_standard_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "azimuth_simple_standard_tests.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in azimuth_simple_standard_tests.mbt"
echo "All tests compiled successfully!"