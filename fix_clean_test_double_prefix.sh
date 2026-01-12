#!/bin/bash

# 修复 clean_test 包所有测试文件的导入语法（修复重复前缀问题）
echo "开始修复 clean_test 测试文件的重复前缀问题..."

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
    
    # 处理文件内容，修复重复前缀问题
    sed -e 's/@clean_test\.@clean_test\./@clean_test./g' \
        -e 's/@azimuth\.@azimuth\./@azimuth./g' \
        -e '/^$/d' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
done

echo "clean_test 重复前缀问题修复完成！"