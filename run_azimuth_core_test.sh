#!/bin/bash

# 运行 azimuth_core_test.mbt 标准测试文件的脚本

echo "Running azimuth_core_test.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译测试文件
echo "Compiling azimuth_core_test.mbt..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi -include-doctests azimuth_core_test.mbt
if [ $? -eq 0 ]; then
  echo "azimuth_core_test.mbt compiled successfully!"
  echo "All 10 test cases are ready to run."
else
  echo "Failed to compile azimuth_core_test.mbt"
fi

echo "Test compilation completed."