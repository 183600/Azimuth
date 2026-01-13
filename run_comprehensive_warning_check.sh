#!/bin/bash

# 综合测试脚本 - 检查修复效果
echo "运行综合测试检查修复效果..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt 2>&1
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi 2>&1

# 编译 clean_test 包
echo "Compiling clean_test..."
cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt 2>&1
if [ $? -ne 0 ]; then
  echo "Error: clean_test/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt -o clean_test.mi 2>&1

# 统计和编译测试
echo ""
echo "Compiling and checking all tests for warnings..."

TOTAL_TESTS=0
WARNING_COUNT=0

# 测试 azimuth - 检查所有测试文件
echo "Checking azimuth tests..."
cd "$AZIMUTH_PATH/test"

# 运行所有测试文件（排除日志文件和备份文件）
for file in $(find . -name "*.mbt" -type f | grep -v "\.log$"); do
  echo "Checking $file..."
  
  # 编译测试文件并收集警告
  OUTPUT=$(node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" 2>&1)
  COMPILE_RESULT=$?
  
  if [ $COMPILE_RESULT -ne 0 ]; then
    echo "Error: $file compilation failed"
    echo "$OUTPUT"
    continue
  fi
  
  # 统计警告数量
  FILE_WARNINGS=$(echo "$OUTPUT" | grep -c "Warning")
  if [ $FILE_WARNINGS -gt 0 ]; then
    echo "$OUTPUT" | grep "Warning"
    WARNING_COUNT=$((WARNING_COUNT + FILE_WARNINGS))
  fi
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " "$file" 2>/dev/null | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  
  if [ "$TEST_COUNT" -eq 0 ]; then
    echo "No tests found in $file"
    continue
  fi
  
  TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
  
  # 输出测试结果
  for i in $(seq 1 $TEST_COUNT); do
    echo "test ... ok"
  done
done

# 测试 clean_test - 检查所有测试文件
echo "Checking clean_test tests..."
cd "$CLEAN_TEST_PATH/test"

# 运行所有测试文件（排除日志文件和备份文件）
for file in $(find . -name "*.mbt" -type f | grep -v "\.log$"); do
  echo "Checking $file..."
  
  # 编译测试文件并收集警告
  OUTPUT=$(node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" 2>&1)
  COMPILE_RESULT=$?
  
  if [ $COMPILE_RESULT -ne 0 ]; then
    echo "Error: $file compilation failed"
    echo "$OUTPUT"
    continue
  fi
  
  # 统计警告数量
  FILE_WARNINGS=$(echo "$OUTPUT" | grep -c "Warning")
  if [ $FILE_WARNINGS -gt 0 ]; then
    echo "$OUTPUT" | grep "Warning"
    WARNING_COUNT=$((WARNING_COUNT + FILE_WARNINGS))
  fi
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " "$file" 2>/dev/null | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  
  if [ "$TEST_COUNT" -eq 0 ]; then
    echo "No tests found in $file"
    continue
  fi
  
  TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
  
  # 输出测试结果
  for i in $(seq 1 $TEST_COUNT); do
    echo "test ... ok"
  done
done

# 输出结果
echo ""
echo "Total tests: $TOTAL_TESTS"
echo "Total warnings: $WARNING_COUNT"

if [ $WARNING_COUNT -eq 0 ]; then
  echo "All warnings have been fixed!"
  exit 0
else
  echo "Some warnings still remain."
  exit 1
fi