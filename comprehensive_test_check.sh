#!/bin/bash

echo "Running comprehensive test check..."

# 进入 src 目录
cd src

# 编译 azimuth 包
echo "Checking azimuth package..."
cd azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core -w +a lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth package has issues!"
else
  echo "azimuth package is OK"
fi

# 生成 azimuth.mi 文件
echo "Generating azimuth.mi..."
node ../../moonc.js check -pkg azimuth -std-path ../../core -o azimuth.mi lib.mbt

# 编译所有测试文件
echo "Checking azimuth test files..."
cd test
# 一次性编译所有测试文件，就像 moon_test_real 一样
node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -i ../azimuth.mi *.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth test package has issues!"
  # 如果整体编译失败，再逐个检查文件
  for file in *.mbt; do
    if [ -f "$file" ]; then
      echo "Checking $file..."
      node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -i ../azimuth.mi "$file"
      if [ $? -ne 0 ]; then
        echo "ERROR: $file has issues!"
      else
        echo "$file is OK"
      fi
    fi
  done
else
  echo "azimuth test package is OK"
  # 逐个检查文件状态
  for file in *.mbt; do
    if [ -f "$file" ]; then
      echo "Checking $file..."
      node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -i ../azimuth.mi "$file"
      if [ $? -ne 0 ]; then
        echo "ERROR: $file has issues!"
      else
        echo "$file is OK"
      fi
    fi
  done
fi

# 检查 clean_test 包
echo "Checking clean_test package..."
cd ../../clean_test
node ../../moonc.js check -pkg clean_test -std-path ../../core -w +a lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: clean_test package has issues!"
else
  echo "clean_test package is OK"
fi

# 生成 clean_test.mi 文件
echo "Generating clean_test.mi..."
node ../../moonc.js check -pkg clean_test -std-path ../../core -o clean_test.mi lib.mbt

# 编译所有 clean_test 测试文件
echo "Checking clean_test test files..."
cd test
# 一次性编译所有测试文件
node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -i ../clean_test.mi *.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: clean_test test package has issues!"
  # 如果整体编译失败，再逐个检查文件
  for file in *.mbt; do
    if [ -f "$file" ]; then
      echo "Checking $file..."
      node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -i ../clean_test.mi "$file"
      if [ $? -ne 0 ]; then
        echo "ERROR: $file has issues!"
      else
        echo "$file is OK"
      fi
    fi
  done
else
  echo "clean_test test package is OK"
  # 逐个检查文件状态
  for file in *.mbt; do
    if [ -f "$file" ]; then
      echo "Checking $file..."
      node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -i ../clean_test.mi "$file"
      if [ $? -ne 0 ]; then
        echo "ERROR: $file has issues!"
      else
        echo "$file is OK"
      fi
    fi
  done
fi

echo "Comprehensive test check complete."