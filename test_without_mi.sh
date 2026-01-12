#!/bin/bash

echo "Running test check without .mi generation..."

# 进入 src 目录
cd src

# 编译 azimuth 包
echo "Checking azimuth package..."
cd azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core -w +a lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth package has issues!"
  exit 1
else
  echo "azimuth package is OK"
fi

# 编译所有测试文件（不使用 .mi 文件）
echo "Checking azimuth test files..."
cd test
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Checking $file..."
    node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -include-doctests -w +a "$file"
    if [ $? -ne 0 ]; then
      echo "ERROR: $file has issues!"
      exit 1
    else
      echo "$file is OK"
    fi
  fi
done

# 检查 clean_test 包
echo "Checking clean_test package..."
cd ../../clean_test
node ../../moonc.js check -pkg clean_test -std-path ../../core -w +a lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: clean_test package has issues!"
  exit 1
else
  echo "clean_test package is OK"
fi

# 编译所有 clean_test 测试文件（不使用 .mi 文件）
echo "Checking clean_test test files..."
cd test
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Checking $file..."
    node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -include-doctests -w +a "$file"
    if [ $? -ne 0 ]; then
      echo "ERROR: $file has issues!"
      exit 1
    else
      echo "$file is OK"
    fi
  fi
done

echo "All tests passed without .mi generation!"