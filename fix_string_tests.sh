#!/bin/bash

# 找到所有使用 assert_eq 的测试文件
files=$(grep -l "assert_eq" /home/runner/work/Azimuth/Azimuth/src/azimuth/test/*.mbt | grep -v "test_helper.mbt")

# 为每个文件修复字符串测试
for file in $files; do
    echo "Processing string tests in $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 读取文件内容并替换字符串测试
    sed 's/assert_eq("\([^"]*\)", @azimuth\.greet("\([^"]*\)")/assert_eq_string("\1", @azimuth.greet("\2"))/g' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
    
    echo "Fixed string tests in $file"
done

echo "Done!"