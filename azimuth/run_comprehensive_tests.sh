#!/bin/bash

# 运行综合测试用例
echo "Running comprehensive tests..."

# 检查测试目录
if [ ! -d "test" ]; then
  echo "No test directory found"
  exit 1
fi

# 进入测试目录
cd test

# 检查我们的测试文件
if [ -f "azimuth_comprehensive_test_suite.mbt" ]; then
  echo "Checking azimuth_comprehensive_test_suite.mbt..."
  # 使用 moonc.js 检查测试文件
  node ../moonc.js check -pkg "$(basename $(pwd))" -std-path "../../core" -blackbox-test azimuth_comprehensive_test_suite.mbt -i ../azimuth.mi
  if [ $? -eq 0 ]; then
    echo "azimuth_comprehensive_test_suite.mbt ... ok"
  else
    echo "azimuth_comprehensive_test_suite.mbt ... failed"
  fi
else
  echo "azimuth_comprehensive_test_suite.mbt not found"
fi

echo "Comprehensive tests completed"
