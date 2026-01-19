#!/bin/bash

# 运行简单核心测试用例的脚本
echo "Running simple core tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译 azimuth 包
echo "Compiling azimuth package..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt simple_core_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " simple_core_tests.mbt | wc -l)
echo ""
echo "Successfully compiled $TEST_COUNT test cases in simple_core_tests.mbt"
echo ""

# 提取测试名称
echo "Test cases found:"
grep "^test " simple_core_tests.mbt | sed 's/test "\(.*\)" {/- \1/'
echo ""

echo "All simple core tests compiled successfully!"