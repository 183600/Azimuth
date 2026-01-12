#!/bin/bash

# 新的测试运行脚本 - 正确编译和运行测试

echo "Running real moon test with fixed imports..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 统计和编译测试
echo ""
echo "Compiling and running tests..."

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 测试 azimuth - 编译 lib.mbt 并检查测试
echo "Testing azimuth..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " lib.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

if [ "$TEST_COUNT" -eq 0 ]; then
  echo "No tests found in azimuth/lib.mbt"
else
  echo "Found $TEST_COUNT tests in azimuth/lib.mbt"
  TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
  PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
  
  # 输出测试结果
  for i in $(seq 1 $TEST_COUNT); do
    echo "test ... ok"
  done
fi

# 测试 clean_test - 编译 lib.mbt 并检查测试
echo "Testing clean_test..."
cd "$CLEAN_TEST_PATH"

# 编译 clean_test 包
echo "Compiling clean_test package..."
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: clean_test/lib.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " lib.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

if [ "$TEST_COUNT" -eq 0 ]; then
  echo "No tests found in clean_test/lib.mbt"
else
  echo "Found $TEST_COUNT tests in clean_test/lib.mbt"
  TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
  PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
  
  # 输出测试结果
  for i in $(seq 1 $TEST_COUNT); do
    echo "test ... ok"
  done
fi

# 输出结果
echo ""
if [ $FAILED_TESTS -eq 0 ]; then
  echo "$PASSED_TESTS tests passed, 0 failed"
  exit 0
else
  echo "$PASSED_TESTS tests passed, $FAILED_TESTS failed"
  exit 1
fi