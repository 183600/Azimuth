#!/bin/bash

# 详细的调试测试脚本，显示所有编译错误和警告

echo "=== 详细调试测试脚本 ==="
echo ""

# 检查 azimuth 包
echo "1. 检查 azimuth/lib.mbt..."
cd src/azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core lib.mbt
echo ""

# 检查 azimuth 测试文件
echo "2. 检查 azimuth 测试文件..."
cd test

# 首先检查 test_helper.mbt
echo "检查 test_helper.mbt..."
node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core test_helper.mbt
echo ""

# 检查其他测试文件
for file in *.mbt; do
  if [ "$file" != "test_helper.mbt" ]; then
    echo "检查 $file..."
    node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core "$file"
    echo ""
  fi
done

# 检查 clean_test 包
echo "3. 检查 clean_test/lib.mbt..."
cd ../../clean_test
node ../../moonc.js check -pkg clean_test -std-path ../../core lib.mbt
echo ""

# 检查 clean_test 测试文件
echo "4. 检查 clean_test 测试文件..."
cd test

for file in *.mbt; do
  echo "检查 $file..."
  node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core "$file"
  echo ""
done

echo "=== 检查完成 ==="