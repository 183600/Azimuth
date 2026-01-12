#!/bin/bash

# 修复测试文件中的包引用问题

echo "Fixing package references in test files..."

# 修复 azimuth 测试文件中的包引用
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 删除文件开头多余的 "test" 行
    sed -i '1{/^test$/d;}' "$file"
    
    # 在函数调用前添加 @azimuth. 前缀
    sed -i 's/\b(add\|multiply\|greet\|assert_eq\|assert_eq_string\|assert_true\|assert_false)(/@azimuth.\1(/g' "$file"
  fi
done

echo "Fixed azimuth test files"

# 修复 clean_test 测试文件中的包引用
cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 删除文件开头多余的 "test" 行
    sed -i '1{/^test$/d;}' "$file"
    
    # 在函数调用前添加 @clean_test. 前缀
    sed -i 's/\b(add\|multiply\|greet\|assert_eq\|assert_eq_string\|assert_true\|assert_false)(/@clean_test.\1(/g' "$file"
  fi
done

echo "Fixed clean_test test files"

echo "All package references fixed"