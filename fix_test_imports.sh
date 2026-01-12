#!/bin/bash

# 修复所有测试文件的导入语法
echo "开始修复测试文件的导入语法..."

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

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
    sed -e 's/^use azimuth::{.*}$//' \
        -e 's/^use clean_test::{.*}$//' \
        -e 's/\<add(/@azimuth.add(/g' \
        -e 's/\<multiply(/@azimuth.multiply(/g' \
        -e 's/\<greet(/@azimuth.greet(/g' \
        -e 's/\<assert_eq(/@azimuth.assert_eq(/g' \
        -e 's/\<assert_eq_string(/@azimuth.assert_eq_string(/g' \
        -e 's/\<assert_true(/@azimuth.assert_true(/g' \
        -e 's/\<assert_false(/@azimuth.assert_false(/g' \
        -e '/^$/d' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
done

echo "所有测试文件修复完成！"