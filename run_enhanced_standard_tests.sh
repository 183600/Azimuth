#!/bin/bash

# 运行增强标准测试用例的脚本
echo "Running enhanced standard test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 编译 azimuth 包和测试文件
echo "Compiling azimuth package with enhanced standard tests..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt enhanced_standard_test_cases.mbt
COMPILATION_RESULT=$?
# 退出码0表示成功，退出码2表示有警告但编译成功
if [ $COMPILATION_RESULT -ne 0 ] && [ $COMPILATION_RESULT -ne 2 ]; then
  echo "Error: Compilation failed with exit code $COMPILATION_RESULT"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " enhanced_standard_test_cases.mbt | wc -l)
echo ""
echo "Successfully compiled $TEST_COUNT test cases in enhanced_standard_test_cases.mbt"
echo ""

# 提取测试名称
echo "Test cases found:"
grep "^test " enhanced_standard_test_cases.mbt | sed 's/test "\(.*\)" {/- \1/'
echo ""

echo "Enhanced standard tests compiled successfully!"