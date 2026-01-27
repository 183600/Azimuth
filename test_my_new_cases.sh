#!/bin/bash

# 测试新添加的标准测试用例
echo "Testing new standard test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译 azimuth 包
echo "Compiling azimuth package..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 编译新添加的测试文件
echo "Compiling new standard test cases..."
cd "$PROJECT_ROOT"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" src/azimuth/new_standard_test_cases.mbt
if [ $? -ne 0 ]; then
  echo "Error: new_standard_test_cases.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " src/azimuth/new_standard_test_cases.mbt | wc -l)
echo ""
echo "Successfully compiled $TEST_COUNT test cases in new_standard_test_cases.mbt"
echo ""

# 提取测试名称
echo "Test cases found:"
grep "^test " src/azimuth/new_standard_test_cases.mbt | sed 's/test "\(.*\)".*/- \1/'
echo ""

echo "All new standard test cases compiled successfully!"