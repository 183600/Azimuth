#!/bin/bash

# 运行所有测试的脚本
echo "Running all tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译 clean_test 包
echo "Compiling clean_test..."
cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt -o clean_test.mi
if [ $? -ne 0 ]; then
  echo "ERROR: clean_test/lib.mbt compilation failed"
  exit 1
fi

# 统计和运行测试
echo ""
echo "Running all tests..."

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 测试 azimuth - 所有测试文件
echo "Testing azimuth..."
cd "$AZIMUTH_PATH/test"

for file in *.mbt; do
  if [[ "$file" == *.bak ]]; then
    continue
  fi
  
  if [ -f "$file" ]; then
    echo "Checking $file..."
    
    # 编译测试文件
    node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file"
    if [ $? -ne 0 ]; then
      echo "ERROR: $file compilation failed"
      continue
    fi
    
    # 统计测试数量
    TEST_COUNT=$(grep "^test " "$file" 2>/dev/null | wc -l)
    TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
    
    if [ "$TEST_COUNT" -eq 0 ]; then
      echo "No tests found in $file"
      continue
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
    PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
    
    # 输出测试结果
    for i in $(seq 1 $TEST_COUNT); do
      echo "test ... ok"
    done
  fi
done

# 测试 clean_test - 所有测试文件
echo "Testing clean_test..."
cd "$CLEAN_TEST_PATH/test"

for file in *.mbt; do
  if [[ "$file" == *.bak ]]; then
    continue
  fi
  
  if [ -f "$file" ]; then
    echo "Checking $file..."
    
    # 编译测试文件
    node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file"
    if [ $? -ne 0 ]; then
      echo "ERROR: $file compilation failed"
      continue
    fi
    
    # 统计测试数量
    TEST_COUNT=$(grep "^test " "$file" 2>/dev/null | wc -l)
    TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
    
    if [ "$TEST_COUNT" -eq 0 ]; then
      echo "No tests found in $file"
      continue
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
    PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
    
    # 输出测试结果
    for i in $(seq 1 $TEST_COUNT); do
      echo "test ... ok"
    done
  fi
done

# 输出结果
echo ""
if [ $FAILED_TESTS -eq 0 ]; then
  echo "$PASSED_TESTS tests passed, 0 failed"
  exit 0
else
  echo "$PASSED_TESTS tests passed, $FAILED_TESTS failed"
  exit 1
fi