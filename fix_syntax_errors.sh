#!/bin/bash

# 修复测试文件中的语法错误
echo "Fixing syntax errors in test files..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 函数：修复单个文件中的语法错误
fix_syntax_errors() {
  local file_path="$1"
  local temp_file="${file_path}.tmp"
  
  # 检查文件是否存在
  if [ ! -f "$file_path" ]; then
    return 0
  fi
  
  # 跳过备份文件和日志文件
  if [[ "$file_path" =~ \.bak$ ]] || [[ "$file_path" =~ \.log$ ]]; then
    return 0
  fi
  
  # 创建临时文件
  cp "$file_path" "$temp_file"
  
  # 修复重复的前缀
  sed -i 's/@azimuth\.@azimuth\./@azimuth\./g' "$temp_file"
  sed -i 's/@clean_test\.@clean_test\./@clean_test\./g' "$temp_file"
  
  # 如果文件有变化，则替换原文件
  if ! diff -q "$file_path" "$temp_file" > /dev/null 2>&1; then
    mv "$temp_file" "$file_path"
    echo "Fixed syntax errors in $file_path"
  else
    rm "$temp_file"
  fi
}

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
find "$AZIMUTH_TEST_PATH" -name "*.mbt" ! -path "*/.*" | while read file; do
  fix_syntax_errors "$file"
done

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
find "$CLEAN_TEST_PATH" -name "*.mbt" ! -path "*/.*" | while read file; do
  fix_syntax_errors "$file"
done

echo "Done fixing syntax errors."