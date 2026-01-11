#!/bin/bash

# 找到所有测试文件
files=$(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" | grep -v "test_helper.mbt")

# 为每个文件修复语法错误
for file in $files; do
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 读取文件内容并修复问题
    sed -e 's/assert_eq_string("[^"]*", @azimuth\.greet("[^"]*")))/assert_eq_string("\1", @azimuth.greet("\2"))/g' \
        -e 's/assert_eq("[^"]*", @azimuth\.greet("[^"]*"))/assert_eq_string("\1", @azimuth.greet("\2"))/g' \
        -e 's/@azimuth\.@azimuth\./@azimuth./g' \
        -e 's/\\n/\\\\n/g' \
        -e 's/\\t/\\\\t/g' \
        "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
    
    echo "Fixed syntax errors in $file"
done

echo "Done!"