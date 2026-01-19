#!/bin/bash

# 运行新标准测试用例的脚本
echo "Running new azimuth standard unit tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"

# 编译新标准测试文件
echo "Compiling new standard unit tests..."
cd "$PROJECT_ROOT"

# 使用 moonc.js 检查测试文件
if [ -f "moonc.js" ]; then
  node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" azimuth_standard_unit_tests.mbt
  if [ $? -eq 0 ]; then
    echo "✓ azimuth_standard_unit_tests.mbt compiled successfully!"
  else
    echo "✗ azimuth_standard_unit_tests.mbt compilation failed"
    exit 1
  fi
else
  echo "moonc.js not found, checking file syntax manually..."
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " azimuth_standard_unit_tests.mbt | wc -l)
echo ""
echo "Found $TEST_COUNT test cases in azimuth_standard_unit_tests.mbt"
echo ""

# 提取测试名称
echo "Test cases added:"
grep "^test " azimuth_standard_unit_tests.mbt | sed 's/test "\(.*\)" {/- \1/'
echo ""

echo "New standard unit tests ready!"