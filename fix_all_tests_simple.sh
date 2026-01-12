#!/bin/bash

# 批量修复所有测试文件中的函数调用
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "=== Fixing azimuth test files ==="
cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "Fixing $file..."
    
    # 恢复到原始版本
    if [ -f "$file.bak" ]; then
      cp "$file.bak" "$file"
    fi
    
    # 替换 azimuth. 为 @azimuth.
    sed -i 's/azimuth\./@azimuth\./g' "$file"
  fi
done

# 修复 clean_test 测试文件
echo ""
echo "=== Fixing clean_test test files ==="
cd "$CLEAN_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "Fixing $file..."
    
    # 恢复到原始版本
    if [ -f "$file.bak" ]; then
      cp "$file.bak" "$file"
    fi
    
    # 替换 clean_test. 为 @clean_test.
    sed -i 's/clean_test\./@clean_test\./g' "$file"
  fi
done

echo ""
echo "All test files have been fixed!"