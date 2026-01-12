#!/bin/bash

# 批量修复测试文件中的函数调用问题
# 将 add, multiply, greet 替换为 azimuth::add, azimuth::multiply, azimuth::greet
# 将 clean_test 包中的函数替换为 clean_test::add, clean_test::multiply, clean_test::greet

echo "开始修复测试文件函数调用问题..."

# 修复 azimuth 测试文件
for file in src/azimuth/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        # 替换函数调用为完全限定名称
        sed -i 's/\badd(/azimuth::add(/g' "$file"
        sed -i 's/\bmultiply(/azimuth::multiply(/g' "$file"
        sed -i 's/\bgreet(/azimuth::greet(/g' "$file"
        sed -i 's/\bassert_eq(/azimuth::assert_eq(/g' "$file"
        sed -i 's/\bassert_eq_string(/azimuth::assert_eq_string(/g' "$file"
        sed -i 's/\bassert_true(/azimuth::assert_true(/g' "$file"
        sed -i 's/\bassert_false(/azimuth::assert_false(/g' "$file"
    fi
done

# 修复 clean_test 测试文件  
for file in src/clean_test/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        # 替换函数调用为完全限定名称
        sed -i 's/\badd(/clean_test::add(/g' "$file"
        sed -i 's/\bmultiply(/clean_test::multiply(/g' "$file"
        sed -i 's/\bgreet(/clean_test::greet(/g' "$file"
        sed -i 's/\bassert_eq(/clean_test::assert_eq(/g' "$file"
        sed -i 's/\bassert_eq_string(/clean_test::assert_eq_string(/g' "$file"
        sed -i 's/\bassert_true(/clean_test::assert_true(/g' "$file"
        sed -i 's/\bassert_false(/clean_test::assert_false(/g' "$file"
    fi
done

echo "修复完成！"