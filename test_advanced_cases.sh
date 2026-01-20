#!/bin/bash

# 测试新添加的高级测试用例

echo "Testing advanced test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
TEST_FILE="advanced_test_cases.mbt"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth with advanced test cases..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译新添加的测试文件
echo "Compiling advanced test file: $TEST_FILE"
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi -include-doctests "$TEST_FILE"
if [ $? -eq 0 ]; then
  echo "Success: $TEST_FILE compiled successfully!"
  echo "Advanced test cases are working correctly."
else
  echo "Error: $TEST_FILE compilation failed"
  exit 1
fi

echo "Test completed."