#!/bin/bash

echo "Testing azimuth package..."
cd /home/runner/work/Azimuth/Azimuth/src

# 首先编译主包
echo "Compiling azimuth package..."
node ../moonc.js build-package -pkg azimuth -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core -o _build/azimuth.mi azimuth/lib.mbt

# 生成测试信息
echo "Generating test info..."
node ../moonc.js gen-test-info -json azimuth/test/simple_test.mbt > _build/test_info.json

echo "Testing clean_test package..."
cd /home/runner/work/Azimuth/Azimuth/src

# 编译clean_test包
echo "Compiling clean_test package..."
node ../moonc.js build-package -pkg clean_test -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core -o _build/clean_test.mi clean_test/lib.mbt