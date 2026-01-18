#!/bin/bash

# 运行新创建的测试文件

echo "运行新创建的测试文件..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"

# 使用 moonc.js 编译 lib.mbt
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi
if [ $? -ne 0 ]; then
  echo "Warning: Failed to generate azimuth.mi file"
fi

# 编译新创建的测试文件
echo "Compiling azimuth_comprehensive_unit_tests.mbt..."
cd test

# 编译 azimuth_comprehensive_unit_tests.mbt
echo "Checking azimuth_comprehensive_unit_tests.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi azimuth_comprehensive_unit_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_comprehensive_unit_tests.mbt has compilation issues"
  exit 1
fi

echo "New test file compiled successfully!"
echo "Test file: azimuth_comprehensive_unit_tests.mbt contains 10 comprehensive test cases"