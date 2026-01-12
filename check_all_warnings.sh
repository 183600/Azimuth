#!/bin/bash

# 检查所有源文件中的警告
echo "Checking warnings in source files..."

# 检查 azimuth/lib.mbt
echo "=== Checking azimuth/lib.mbt ==="
cd src/azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core -w +a lib.mbt 2>&1
echo ""

# 检查 clean_test/lib.mbt
echo "=== Checking clean_test/lib.mbt ==="
cd ../clean_test
node ../../moonc.js check -pkg clean_test -std-path ../../core -w +a lib.mbt 2>&1
echo ""

# 检查所有测试文件
echo "=== Checking test files ==="
cd ../azimuth/test
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "--- Checking $file ---"
    if [ "$file" = "test_helper.mbt" ]; then
      # test_helper.mbt 单独编译
      node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -w +a "$file" 2>&1
    else
      # 其他测试文件包含 test_helper.mbt
      node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -w +a -i ../azimuth.mi test_helper.mbt "$file" 2>&1
    fi
    echo ""
  fi
done

cd ../../clean_test/test
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "--- Checking $file ---"
    if [ "$file" = "test_helper.mbt" ]; then
      # test_helper.mbt 单独编译
      node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -w +a -i ../clean_test.mi "$file" 2>&1
    else
      # 其他测试文件包含 test_helper.mbt
      node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -w +a -i ../clean_test.mi test_helper.mbt "$file" 2>&1
    fi
    echo ""
  fi
done

cd ../../..