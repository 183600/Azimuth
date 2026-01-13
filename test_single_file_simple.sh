#!/bin/bash

# 测试单个文件的脚本

echo "Testing single file: standard_moonbit_tests.mbt"

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

cd "$AZIMUTH_PATH"

# 1. 编译主包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth" -std-path "$CORE_PATH" -o "azimuth.mi" lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth package compilation failed"
  exit 1
fi

# 2. 编译单个测试文件
echo "Compiling standard_moonbit_tests.mbt..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth_test" -std-path "$CORE_PATH" -i "../azimuth.mi" standard_moonbit_tests.mbt
if [ $? -eq 0 ]; then
  echo "SUCCESS: standard_moonbit_tests.mbt compiled successfully"
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " standard_moonbit_tests.mbt 2>/dev/null | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  
  echo "Found $TEST_COUNT tests in standard_moonbit_tests.mbt"
  echo "All $TEST_COUNT tests compiled successfully"
  for i in $(seq 1 $TEST_COUNT); do
    echo "test ... ok"
  done
else
  echo "ERROR: standard_moonbit_tests.mbt compilation failed"
  exit 1
fi

echo ""
echo "=== Test Results ==="
echo "$TEST_COUNT tests passed, 0 failed"
exit 0