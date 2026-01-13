#!/bin/bash

# 批量删除测试文件中的重复函数定义
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 函数：删除文件中的重复函数定义
remove_duplicate_functions() {
  local file="$1"
  echo "Processing $file..."
  
  # 创建临时文件
  local temp_file=$(mktemp)
  
  # 查找第一个测试函数的行号
  local first_test_line=$(grep -n "^test " "$file" | head -1 | cut -d: -f1)
  
  if [ -z "$first_test_line" ]; then
    echo "No test functions found in $file"
    rm -f "$temp_file"
    return 1
  fi
  
  # 提取从第一个测试函数开始到文件末尾的内容
  tail -n +$first_test_line "$file" > "$temp_file"
  
  # 替换原文件
  mv "$temp_file" "$file"
  
  echo "Fixed $file"
  return 0
}

# 处理 azimuth 测试文件
echo "=== Fixing azimuth test files ==="
cd "$AZIMUTH_TEST_PATH"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    remove_duplicate_functions "$file"
  fi
done

# 处理 clean_test 测试文件
echo ""
echo "=== Fixing clean_test test files ==="
cd "$CLEAN_TEST_PATH"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    remove_duplicate_functions "$file"
  fi
done

echo ""
echo "All test files have been fixed!"