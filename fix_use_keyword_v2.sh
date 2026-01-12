#!/bin/bash

# 修复所有使用use关键字的测试文件

cd /home/runner/work/Azimuth/Azimuth

# 查找所有使用use关键字的测试文件
TEST_FILES=$(find src -name "*.mbt" -exec grep -l "^use " {} \;)

for file in $TEST_FILES; do
  echo "Fixing $file..."
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 处理文件内容
  sed '/^use /d' "$file" > "$temp_file"
  
  # 将所有函数调用替换为带包前缀的形式
  sed -i 's/assert_eq(/azimuth::assert_eq(/g' "$temp_file"
  sed -i 's/assert_eq_string(/azimuth::assert_eq_string(/g' "$temp_file"
  sed -i 's/assert_true(/azimuth::assert_true(/g' "$temp_file"
  sed -i 's/assert_false(/azimuth::assert_false(/g' "$temp_file"
  
  # 修复双重前缀问题
  sed -i 's/azimuth::azimuth::/azimuth::/g' "$temp_file"
  
  # 替换原文件
  mv "$temp_file" "$file"
done

echo "Fixed all test files"