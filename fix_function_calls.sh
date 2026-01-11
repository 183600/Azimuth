#!/bin/bash

# 找到所有测试文件
files=$(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" | grep -v "test_helper.mbt")

# 为每个文件修复缺少 @azimuth 前缀的函数调用
for file in $files; do
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 读取文件内容并修复问题
    sed -e 's/\badd(/@azimuth.add(/g' \
        -e 's/\bmultiply(/@azimuth.multiply(/g' \
        -e 's/\bgreet(/@azimuth.greet(/g' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
    
    echo "Fixed function calls in $file"
done

echo "Done!"