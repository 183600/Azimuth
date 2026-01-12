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
    sed -i 's/add(/@azimuth.add(/g' "$file"
    sed -i 's/multiply(/@azimuth.multiply(/g' "$file"
    sed -i 's/greet(/@azimuth.greet(/g' "$file"
    sed -i 's/assert_eq(/@azimuth.assert_eq(/g' "$file"
    sed -i 's/assert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
    sed -i 's/assert_true(/@azimuth.assert_true(/g' "$file"
    sed -i 's/assert_false(/@azimuth.assert_false(/g' "$file"
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
    sed -i 's/add(/@clean_test.add(/g' "$file"
    sed -i 's/multiply(/@clean_test.multiply(/g' "$file"
    sed -i 's/greet(/@clean_test.greet(/g' "$file"
    sed -i 's/assert_eq(/@clean_test.assert_eq(/g' "$file"
    sed -i 's/assert_eq_string(/@clean_test.assert_eq_string(/g' "$file"
    sed -i 's/assert_true(/@clean_test.assert_true(/g' "$file"
    sed -i 's/assert_false(/@clean_test.assert_false(/g' "$file"
  fi
done

echo "Fixed clean_test test files"

echo "All package references fixed"