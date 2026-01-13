#!/bin/bash

echo "=== Testing MoonBit Package Import Fixes ==="
cd /home/runner/work/Azimuth/Azimuth

# 1. 测试 azimuth 包
echo "1. Testing azimuth package..."
cd azimuth

# 编译主包
echo "Compiling azimuth main package..."
node ../moonc.js check -pkg azimuth -std-path ../core lib.mbt

# 编译测试包
echo "Compiling azimuth test package..."
cd test
node ../../moonc.js check -pkg azimuth/test -std-path ../../core -i ../azimuth.mi simple_test.mbt

if [ $? -eq 0 ]; then
  echo "✓ azimuth test package compiled successfully"
else
  echo "✗ azimuth test package compilation failed"
fi

# 2. 测试 clean_test 包
echo ""
echo "2. Testing clean_test package..."
cd ../../clean_test

# 编译主包
echo "Compiling clean_test main package..."
node ../moonc.js check -pkg clean_test -std-path ../core lib.mbt

# 编译测试包
echo "Compiling clean_test test package..."
cd test
node ../../moonc.js check -pkg clean_test/test -std-path ../../core -i ../clean_test.mi simple_test.mbt

if [ $? -eq 0 ]; then
  echo "✓ clean_test test package compiled successfully"
else
  echo "✗ clean_test test package compilation failed"
fi

echo ""
echo "=== Test Complete ==="