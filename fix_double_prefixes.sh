#!/bin/bash

# 修复所有测试文件中的双重前缀问题

cd /home/runner/work/Azimuth/Azimuth

# 查找所有测试文件
TEST_FILES=$(find src -name "*.mbt")

for file in $TEST_FILES; do
  # 检查文件是否包含双重前缀
  if grep -q "azimuth::azimuth::" "$file"; then
    echo "Fixing double prefixes in $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 修复双重前缀问题
    sed 's/azimuth::azimuth::/azimuth::/g' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "Fixed double prefixes in all test files"