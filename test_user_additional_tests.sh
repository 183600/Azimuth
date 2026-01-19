#!/bin/bash

# 运行用户添加的测试用例的脚本
echo "Running user additional tests..."

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

# 编译用户添加的测试文件
echo "Compiling user additional tests..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi user_additional_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: user_additional_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " user_additional_tests.mbt | wc -l)
echo ""
echo "Successfully compiled $TEST_COUNT test cases in user_additional_tests.mbt"
echo ""

# 提取测试名称
echo "Test cases found:"
grep "^test " user_additional_tests.mbt | sed 's/test "\(.*\)" {/- \1/'
echo ""

echo "All user additional tests compiled successfully!"