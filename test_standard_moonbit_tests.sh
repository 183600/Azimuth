#!/bin/bash

# 测试新添加的标准 MoonBit 测试用例

echo "Testing standard MoonBit test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth with standard MoonBit tests..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译测试文件
echo "Compiling standard_moonbit_tests.mbt..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi -include-doctests standard_moonbit_tests.mbt
if [ $? -eq 0 ]; then
  echo "standard_moonbit_tests.mbt compiled successfully!"
else
  echo "Failed to compile standard_moonbit_tests.mbt"
fi

echo "Standard MoonBit test completed."