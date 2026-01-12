#!/bin/bash

# 验证所有测试文件都能编译通过的脚本
echo "Verifying all test files compile correctly..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译 clean_test 包
echo "Compiling clean_test..."
cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt -o clean_test.mi
if [ $? -ne 0 ]; then
  echo "ERROR: clean_test/lib.mbt compilation failed"
  exit 1
fi

# 验证 azimuth 测试文件
echo ""
echo "Verifying azimuth test files..."
cd "$AZIMUTH_PATH/test"

TOTAL_FILES=0
PASSED_FILES=0
FAILED_FILES=0

for file in *.mbt; do
  if [[ "$file" == *.bak ]]; then
    continue
  fi
  
  if [ -f "$file" ]; then
    TOTAL_FILES=$((TOTAL_FILES + 1))
    echo -n "Checking $file... "
    
    # 编译测试文件
    node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" >/dev/null 2>&1
    if [ $? -eq 0 ]; then
      echo "OK"
      PASSED_FILES=$((PASSED_FILES + 1))
    else
      echo "FAILED"
      FAILED_FILES=$((FAILED_FILES + 1))
      # 显示错误详情
      node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" 2>&1 | head -5
    fi
  fi
done

# 验证 clean_test 测试文件
echo ""
echo "Verifying clean_test test files..."
cd "$CLEAN_TEST_PATH/test"

for file in *.mbt; do
  if [[ "$file" == *.bak ]]; then
    continue
  fi
  
  if [ -f "$file" ]; then
    TOTAL_FILES=$((TOTAL_FILES + 1))
    echo -n "Checking $file... "
    
    # 编译测试文件
    node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" >/dev/null 2>&1
    if [ $? -eq 0 ]; then
      echo "OK"
      PASSED_FILES=$((PASSED_FILES + 1))
    else
      echo "FAILED"
      FAILED_FILES=$((FAILED_FILES + 1))
      # 显示错误详情
      node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" 2>&1 | head -5
    fi
  fi
done

# 输出结果
echo ""
echo "======================================="
echo "Verification Results:"
echo "Total files: $TOTAL_FILES"
echo "Passed: $PASSED_FILES"
echo "Failed: $FAILED_FILES"
echo "======================================="

if [ $FAILED_FILES -eq 0 ]; then
  echo "SUCCESS: All test files compile correctly!"
  exit 0
else
  echo "ERROR: $FAILED_FILES test files still have compilation errors"
  exit 1
fi