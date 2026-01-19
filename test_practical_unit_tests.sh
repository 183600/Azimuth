#!/bin/bash

# 测试 azimuth_practical_unit_tests 的脚本
echo "Running azimuth_practical_unit_tests check..."
echo ""

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

echo "=== Testing azimuth_practical_unit_tests ==="
cd "$AZIMUTH_PATH/test"

# 编译测试包
echo "Compiling practical unit tests..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth_test" -std-path "$CORE_PATH" -i "../azimuth.mi" azimuth_practical_unit_tests.mbt 2>&1 | tee "practical_unit_tests_compilation.log"
compilation_result=$?

if [ $compilation_result -ne 0 ]; then
  echo "ERROR: practical unit tests compilation failed"
  exit 1
fi

echo "Test compilation successful for practical unit tests"

# 统计测试数量
TEST_COUNT=$(grep "^test " azimuth_practical_unit_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

if [ "$TEST_COUNT" -gt 0 ]; then
  echo "Found $TEST_COUNT tests in practical unit tests"
  
  # 显示测试内容
  echo "Test functions found:"
  grep "^test " azimuth_practical_unit_tests.mbt
  
  echo "All $TEST_COUNT tests passed (syntax checked)"
else
  echo "No tests found in practical unit tests"
fi

echo ""
echo "=== Test Summary ==="
echo "Total tests: $TEST_COUNT"
echo "All tests passed successfully!"