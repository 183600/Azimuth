#!/bin/bash

# 简化的测试脚本，专注于检查编译错误
echo "Running simplified test..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"

# 检查语法和导入
echo "Checking syntax and imports..."

# 检查 azimuth 包
echo "Checking azimuth..."
cd src/azimuth

# 使用 moonc check 检查代码
COMPILE_ERRORS=0
node $PROJECT_ROOT/moonc.js check -pkg azimuth -std-path "$PROJECT_ROOT/core" lib.mbt 2>&1 | tee azimuth_compile.log
if [ $? -ne 0 ]; then
  echo "Error: azimuth package check failed"
  COMPILE_ERRORS=$((COMPILE_ERRORS + 1))
fi

# 检查 clean_test 包
echo "Checking clean_test..."
cd ../clean_test

node $PROJECT_ROOT/moonc.js check -pkg clean_test -std-path "$PROJECT_ROOT/core" lib.mbt 2>&1 | tee clean_test_compile.log
if [ $? -ne 0 ]; then
  echo "Error: clean_test package check failed"
  COMPILE_ERRORS=$((COMPILE_ERRORS + 1))
fi

# 检查测试文件
echo "Checking test files..."
cd ../azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Checking $file..."
    node $PROJECT_ROOT/moonc.js check -pkg azimuth_test -std-path "$PROJECT_ROOT/core" -i ../azimuth.mi "$file" 2>&1 | tee "test_$file.log"
    if [ $? -ne 0 ]; then
      echo "Error: test file $file check failed"
      COMPILE_ERRORS=$((COMPILE_ERRORS + 1))
    fi
  fi
done

cd ../../clean_test/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Checking $file..."
    node $PROJECT_ROOT/moonc.js check -pkg clean_test_test -std-path "$PROJECT_ROOT/core" -i ../clean_test.mi "$file" 2>&1 | tee "test_$file.log"
    if [ $? -ne 0 ]; then
      echo "Error: test file $file check failed"
      COMPILE_ERRORS=$((COMPILE_ERRORS + 1))
    fi
  fi
done

# 输出结果
echo ""
if [ $COMPILE_ERRORS -gt 0 ]; then
  echo "Found $COMPILE_ERRORS compilation errors"
  exit 1
else
  echo "No compilation errors found"
  exit 0
fi