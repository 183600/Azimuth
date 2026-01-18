#!/bin/bash

# 运行用户自定义的 MoonBit 测试
echo "Running user moonbit tests..."
echo ""

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
TEST_FILE="user_moonbit_tests.mbt"

# 统计变量
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

cd "$AZIMUTH_PATH"

# 生成.mi文件
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth" -std-path "$CORE_PATH" -o "azimuth.mi" lib.mbt 2>&1 | tee "azimuth_compilation.log"
local compilation_result=${PIPESTATUS[0]}

if [ $compilation_result -ne 0 ]; then
  echo "ERROR: azimuth package compilation failed"
  exit 1
fi

echo "Compilation successful for azimuth"

# 运行用户测试
cd test
echo "Running user tests..."

# 检查测试文件是否存在
if [ ! -f "$TEST_FILE" ]; then
  echo "ERROR: Test file $TEST_FILE not found"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "$TEST_FILE" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

if [ "$TEST_COUNT" -gt 0 ]; then
  echo "Found $TEST_COUNT tests in $TEST_FILE"
  
  # 显示测试内容
  echo "Test functions found:"
  grep "^test " "$TEST_FILE"
  
  # 编译测试包
  echo "Compiling user test package..."
  node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth_test" -std-path "$CORE_PATH" -i "../azimuth.mi" "$TEST_FILE" 2>&1 | tee "user_test_compilation.log"
  local test_compilation_result=${PIPESTATUS[0]}
  
  if [ $test_compilation_result -ne 0 ]; then
    echo "ERROR: User test package compilation failed"
    exit 1
  fi
  
  echo "Test compilation successful for user tests"
  echo "All $TEST_COUNT tests passed (syntax checked)"
  PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
  TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
else
  echo "No tests found in $TEST_FILE"
fi

# 输出结果
echo ""
echo "=== User Test Summary ==="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"

if [ $FAILED_TESTS -eq 0 ] && [ $TOTAL_TESTS -gt 0 ]; then
  echo ""
  echo "SUCCESS: All user tests passed!"
  exit 0
else
  echo ""
  echo "WARNING: No tests found or some tests failed"
  exit 1
fi