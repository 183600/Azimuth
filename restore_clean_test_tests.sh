#!/bin/bash

# 批量修复 clean_test 测试文件中的函数调用
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 clean_test 测试文件
echo "=== Fixing clean_test test files ==="
cd "$CLEAN_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "Fixing $file..."
    
    # 恢复到原始版本
    if [ -f "$file.bak" ]; then
      cp "$file.bak" "$file"
    fi
  fi
done

echo ""
echo "All clean_test test files have been restored to original version!"