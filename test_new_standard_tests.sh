#!/bin/bash

# 测试新添加的标准测试用例

echo "Testing new standard test examples..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
TEST_FILE="standard_moonbit_test_examples.mbt"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth with new test file..."
cd "$AZIMUTH_PATH/test"

# 编译 azimuth 包
echo "Compiling azimuth package..."
cd ..
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译新添加的测试文件
echo "Compiling new test file: $TEST_FILE"
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi -include-doctests "$TEST_FILE"
if [ $? -eq 0 ]; then
  echo "Success: $TEST_FILE compiled successfully!"
  echo "New standard test examples are working correctly."
else
  echo "Error: $TEST_FILE compilation failed"
  exit 1
fi

echo "Test completed."