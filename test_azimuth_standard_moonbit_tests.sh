#!/bin/bash

# 测试新添加的 azimuth_standard_moonbit_tests.mbt 测试用例

echo "Testing azimuth_standard_moonbit_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth with new standard MoonBit tests..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译测试文件
echo "Compiling azimuth_standard_moonbit_tests.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i azimuth.mi azimuth_standard_moonbit_tests.mbt
if [ $? -eq 0 ]; then
  echo "azimuth_standard_moonbit_tests.mbt compiled successfully!"
else
  echo "Failed to compile azimuth_standard_moonbit_tests.mbt"
fi

echo "New azimuth standard MoonBit test completed."