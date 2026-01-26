#!/bin/bash

# 测试新添加的标准测试用例

echo "Testing azimuth_additional_standard_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 测试 azimuth_additional_standard_tests.mbt
echo "Testing azimuth_additional_standard_tests.mbt..."
cd "$PROJECT_ROOT"

# 尝试编译新添加的测试文件
echo "Compiling azimuth_additional_standard_tests.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i "$AZIMUTH_PATH/azimuth.mi" azimuth_additional_standard_tests.mbt

if [ $? -eq 0 ]; then
  echo "azimuth_additional_standard_tests.mbt compiled successfully!"
else
  echo "Failed to compile azimuth_additional_standard_tests.mbt"
  exit 1
fi

echo "Test completed successfully!"