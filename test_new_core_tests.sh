#!/bin/bash

# 测试新创建的核心测试文件
echo "Testing new standard azimuth core tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 进入 azimuth 目录
cd "$AZIMUTH_PATH"

# 编译主包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth" -std-path "$PROJECT_ROOT/core"

if [ $? -ne 0 ]; then
  echo "Error: azimuth package compilation failed"
  exit 1
fi

# 进入测试目录
cd test

# 编译测试包
echo "Compiling azimuth_test package..."
node "../moonc.js" check -pkg "azimuth_test" -std-path "$PROJECT_ROOT/core"

if [ $? -ne 0 ]; then
  echo "Error: azimuth_test package compilation failed"
  exit 1
fi

# 统计新测试文件中的测试数量
echo "Checking new test file..."
if [ -f "standard_azimuth_core_tests.mbt" ]; then
  TEST_COUNT=$(grep "^test " "standard_azimuth_core_tests.mbt" | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  echo "Found $TEST_COUNT tests in standard_azimuth_core_tests.mbt"
  
  # 显示测试名称
  echo "Test names:"
  grep "^test " "standard_azimuth_core_tests.mbt" | sed 's/test "/- /' | sed 's/" {$//'
  
  if [ "$TEST_COUNT" -le 10 ]; then
    echo "✓ Test count is within limit (<= 10)"
  else
    echo "✗ Test count exceeds limit (> 10)"
    exit 1
  fi
else
  echo "Error: standard_azimuth_core_tests.mbt not found"
  exit 1
fi

echo ""
echo "New standard azimuth core tests validation completed successfully!"
echo "All $TEST_COUNT tests are syntactically correct and ready to run."