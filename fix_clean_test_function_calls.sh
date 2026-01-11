#!/bin/bash

# 修复 clean_test 测试文件中缺少前缀的函数调用

echo "修复 clean_test 测试文件中缺少前缀的函数调用..."

# 找到所有 clean_test 的测试文件
find /home/runner/work/Azimuth/Azimuth/src/clean_test/test -name "*.mbt" -type f | while read file; do
    echo "修复 $file 中的函数调用..."
    # 将 multiply( 替换为 @clean_test.multiply(
    sed -i 's/multiply(/@clean_test.multiply(/g' "$file"
    # 将 add( 替换为 @clean_test.add(
    sed -i 's/add(/@clean_test.add(/g' "$file"
    # 将 greet( 替换为 @clean_test.greet(
    sed -i 's/greet(/@clean_test.greet(/g' "$file"
done

echo "修复完成！"