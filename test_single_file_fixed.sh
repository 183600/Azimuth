#!/bin/bash

# 测试单个文件（修复版）

echo "测试单个文件（修复版）..."

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

# 编译单个测试文件
echo "Compiling single test file..."
cd test

# 测试 simple_test.mbt
echo "Checking simple_test.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -pkg-sources azimuth:.. simple_test.mbt
if [ $? -ne 0 ]; then
  echo "Error: simple_test.mbt has compilation issues"
  exit 1
fi

echo "Single test file compiled successfully!"