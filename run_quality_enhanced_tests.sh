#!/bin/bash

# 运行新添加的质量增强测试用例
echo "Running quality enhanced test cases..."

# 编译azimuth包
echo "Compiling azimuth package..."
cd src/azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core lib.mbt quality_enhanced_test_cases.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth compilation failed"
  exit 1
fi

# 生成WASM文件
echo "Generating WASM file..."
node ../../moonc.js build -pkg azimuth -std-path ../../core -target wasm -o ../_build/wasm-gc/debug/test/azimuth/quality_enhanced_test_cases.wasm quality_enhanced_test_cases.mbt

# 返回根目录
cd ../..

echo "Quality enhanced test cases compilation completed successfully."
echo "Note: Actual test execution would require additional WASM runtime setup."