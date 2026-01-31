#!/bin/bash

# 测试用户添加的综合测试用例

echo "Testing user added comprehensive test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth with user added comprehensive tests..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译测试文件
echo "Compiling user_added_comprehensive_tests.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt user_added_comprehensive_tests.mbt
if [ $? -eq 0 ]; then
  echo "user_added_comprehensive_tests.mbt compiled successfully!"
else
  echo "Error: user_added_comprehensive_tests.mbt compilation failed"
  exit 1
fi

echo "User added comprehensive tests completed successfully!"
