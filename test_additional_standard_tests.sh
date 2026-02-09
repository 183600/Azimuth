#!/bin/bash

# 测试新创建的附加标准测试文件

echo "Testing additional standard tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 测试 azimuth_additional_standard_tests.mbt
echo "Testing azimuth_additional_standard_tests.mbt..."
cd "$PROJECT_ROOT"

# 编译 azimuth 包
echo "Compiling azimuth package..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译新的测试文件
echo "Compiling azimuth_additional_standard_tests.mbt..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" -include-doctests azimuth_additional_standard_tests.mbt
if [ $? -eq 0 ]; then
  echo "azimuth_additional_standard_tests.mbt compiled successfully!"
  echo "All tests in azimuth_additional_standard_tests.mbt are syntactically correct."
else
  echo "Failed to compile azimuth_additional_standard_tests.mbt"
  exit 1
fi

echo "Additional standard tests completed successfully."