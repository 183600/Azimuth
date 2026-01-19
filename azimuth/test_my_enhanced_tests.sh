#!/bin/bash

# 测试新创建的增强测试用例
echo "Testing enhanced test cases..."

cd test

if [ -f "azimuth_enhanced_test_cases.mbt" ]; then
  echo "Checking azimuth_enhanced_test_cases.mbt..."
  # 使用 moonc.js 检查测试文件
  node ../moonc.js check -pkg "$(basename $(pwd))" -std-path "../../core" -blackbox-test azimuth_enhanced_test_cases.mbt -i ../azimuth.mi
  if [ $? -eq 0 ]; then
    echo "azimuth_enhanced_test_cases.mbt ... ok"
  else
    echo "azimuth_enhanced_test_cases.mbt ... failed"
  fi
else
  echo "azimuth_enhanced_test_cases.mbt not found"
fi

echo "Test completed."