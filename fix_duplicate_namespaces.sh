#!/bin/bash

# 修复重复的命名空间前缀

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

echo "Fixing duplicate namespace prefixes in azimuth test files..."

# 修复 azimuth 测试文件
cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容
    sed 's/azimuth::azimuth::/azimuth::/g' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "Fixing duplicate namespace prefixes in clean_test test files..."

# 修复 clean_test 测试文件
cd "$CLEAN_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容
    sed 's/clean_test::clean_test::/clean_test::/g' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "All duplicate namespace prefixes fixed!"