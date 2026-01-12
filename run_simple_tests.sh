#!/bin/bash

# 简单的测试运行脚本
echo "Running tests..."

# 编译所有包
echo "Compiling packages..."

# 编译azimuth包
echo "Compiling azimuth..."
cd src/azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth compilation failed"
  exit 1
fi

# 编译clean_test包
echo "Compiling clean_test..."
cd ../clean_test
node ../../moonc.js check -pkg clean_test -std-path ../../core lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: clean_test compilation failed"
  exit 1
fi

# 返回根目录
cd ../..

# 重新生成WASM文件
echo "Generating WASM files..."
node ./moonc.js build -pkg azimuth -std-path ./core -target wasm -o src/_build/wasm-gc/debug/test/azimuth/azimuth.internal_test.wasm src/azimuth/lib.mbt
node ./moonc.js build -pkg clean_test -std-path ./core -target wasm -o src/_build/wasm-gc/debug/test/clean_test/clean_test.internal_test.wasm src/clean_test/lib.mbt

# 运行测试
echo "Running tests..."
node run_wasm_tests.js ./src/_build/wasm-gc/debug/test/azimuth/azimuth.internal_test.wasm ./src/_build/wasm-gc/debug/test/clean_test/clean_test.internal_test.wasm

echo "Test run completed."