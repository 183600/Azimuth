#!/bin/bash

# 精确验证测试修复结果
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

echo "=== 验证修复后的测试结果 ==="

# 检查azimuth包
echo "检查azimuth包..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" > azimuth_check_result.log 2>&1
if [ $? -eq 0 ]; then
  echo "✓ azimuth包编译成功"
else
  echo "✗ azimuth包编译失败"
  cat azimuth_check_result.log
fi

# 检查azimuth测试包
echo "检查azimuth测试包..."
cd "$AZIMUTH_PATH/test"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" > azimuth_test_check_result.log 2>&1
if [ $? -eq 0 ]; then
  echo "✓ azimuth测试包编译成功"
  
  # 统计测试数量
  TEST_COUNT=0
  for test_file in *.mbt; do
    if [ -f "$test_file" ]; then
      file_tests=$(grep "^test " "$test_file" 2>/dev/null | wc -l)
      file_tests=$(echo "$file_tests" | tr -d ' ')
      TEST_COUNT=$((TEST_COUNT + file_tests))
    fi
  done
  echo "  包含 $TEST_COUNT 个测试用例"
else
  echo "✗ azimuth测试包编译失败"
  cat azimuth_test_check_result.log
fi

# 检查clean_test包
echo "检查clean_test包..."
cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" > clean_test_check_result.log 2>&1
if [ $? -eq 0 ]; then
  echo "✓ clean_test包编译成功"
else
  echo "✗ clean_test包编译失败"
  cat clean_test_check_result.log
fi

# 检查clean_test测试包
echo "检查clean_test测试包..."
cd "$CLEAN_TEST_PATH/test"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" > clean_test_test_check_result.log 2>&1
if [ $? -eq 0 ]; then
  echo "✓ clean_test测试包编译成功"
  
  # 统计测试数量
  TEST_COUNT=0
  for test_file in *.mbt; do
    if [ -f "$test_file" ]; then
      file_tests=$(grep "^test " "$test_file" 2>/dev/null | wc -l)
      file_tests=$(echo "$file_tests" | tr -d ' ')
      TEST_COUNT=$((TEST_COUNT + file_tests))
    fi
  done
  echo "  包含 $TEST_COUNT 个测试用例"
else
  echo "✗ clean_test测试包编译失败"
  cat clean_test_test_check_result.log
fi

echo ""
echo "=== 验证完成 ==="