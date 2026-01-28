#!/bin/bash

# 测试新创建的标准测试用例

echo "Testing new standard test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"
TEST_FILE="new_standard_tests.mbt"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth with new test file: $TEST_FILE"
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
echo "Generating .mi file..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 编译新添加的测试文件
echo "Compiling new test file: $TEST_FILE"
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$TEST_FILE"
if [ $? -eq 0 ]; then
  echo "Success: $TEST_FILE compiled successfully!"
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " "$TEST_FILE" | wc -l)
  echo "Successfully compiled $TEST_COUNT test cases in $TEST_FILE"
  
  # 提取测试名称
  echo "Test cases found:"
  grep "^test " "$TEST_FILE" | sed 's/test "\(.*\)"/- \1/'
  
  echo "New standard test cases are working correctly."
else
  echo "Error: $TEST_FILE compilation failed"
  exit 1
fi

echo "Test completed."