#!/bin/bash

# 运行增强的单元测试用例
echo "Running enhanced unit tests..."

# 检查moonc.js是否存在
if [ ! -f "moonc.js" ]; then
  echo "moonc.js not found"
  exit 1
fi

# 检查我们的测试文件
if [ -f "enhanced_unit_tests.mbt" ]; then
  echo "Checking enhanced_unit_tests.mbt..."
  # 使用 moonc.js 检查测试文件
  node moonc.js check -pkg "$(basename $(pwd))" -std-path "../core" enhanced_unit_tests.mbt -i azimuth.mi
  if [ $? -eq 0 ]; then
    echo "enhanced_unit_tests.mbt ... ok"
  else
    echo "enhanced_unit_tests.mbt ... failed"
  fi
else
  echo "enhanced_unit_tests.mbt not found"
fi

echo "Enhanced unit tests completed"