#!/bin/bash

# 修复 clean_test 包所有测试文件的导入语法
echo "开始修复 clean_test 测试文件的导入语法..."

cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

# 修复所有 .mbt 文件，排除备份文件
for file in *.mbt; do
    # 跳过备份文件
    if [[ $file == *.bak* ]]; then
        continue
    fi
    
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容
    sed -e 's/^use clean_test::{.*}$//' \
        -e 's/^use azimuth::{.*}$//' \
        -e 's/\<@clean_test\.add(/@clean_test.add(/g' \
        -e 's/\<@clean_test\.multiply(/@clean_test.multiply(/g' \
        -e 's/\<@clean_test\.greet(/@clean_test.greet(/g' \
        -e 's/\<@clean_test\.assert_eq(/@clean_test.assert_eq(/g' \
        -e 's/\<@clean_test\.assert_eq_string(/@clean_test.assert_eq_string(/g' \
        -e 's/\<@clean_test\.assert_true(/@clean_test.assert_true(/g' \
        -e 's/\<@clean_test\.assert_false(/@clean_test.assert_false(/g' \
        -e 's/\<add(/@clean_test.add(/g' \
        -e 's/\<multiply(/@clean_test.multiply(/g' \
        -e 's/\<greet(/@clean_test.greet(/g' \
        -e 's/\<assert_eq(/@clean_test.assert_eq(/g' \
        -e 's/\<assert_eq_string(/@clean_test.assert_eq_string(/g' \
        -e 's/\<assert_true(/@clean_test.assert_true(/g' \
        -e 's/\<assert_false(/@clean_test.assert_false(/g' \
        -e '/^$/d' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
done

echo "clean_test 所有测试文件修复完成！"
