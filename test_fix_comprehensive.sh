#!/bin/bash

echo "=== Comprehensive MoonBit Test Fix ==="
cd /home/runner/work/Azimuth/Azimuth

# 创建构建目录
mkdir -p _build/test

# 1. 首先编译核心包
echo "1. Compiling core package..."
cd core
node ../moonc.js build-package -pkg core -workspace-path . -std-path . lib.mbt

# 2. 编译azimuth包
echo "2. Compiling azimuth package..."
cd ../azimuth
node ../moonc.js build-package -pkg azimuth -workspace-path . -std-path ../core lib.mbt

# 3. 编译clean_test包
echo "3. Compiling clean_test package..."
cd ../clean_test
node ../moonc.js build-package -pkg clean_test -workspace-path . -std-path ../core lib.mbt

# 4. 测试azimuth包
echo "4. Testing azimuth package..."
node ../moonc.js test -pkg azimuth -workspace-path . -std-path ../core

# 5. 测试clean_test包
echo "5. Testing clean_test package..."
node ../moonc.js test -pkg clean_test -workspace-path . -std-path ../core

echo ""
echo "=== Test Complete ==="