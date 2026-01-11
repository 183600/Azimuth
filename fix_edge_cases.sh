#!/bin/bash

# 修复 additional_edge_cases.mbt 文件
file="/home/runner/work/Azimuth/Azimuth/src/azimuth/test/additional_edge_cases.mbt"

# 创建临时文件
temp_file=$(mktemp)

# 读取文件内容并修复问题
sed -e 's/\badd(/@azimuth.add(/g' \
    -e 's/\bmultiply(/@azimuth.multiply(/g' \
    -e 's/\bgreet(/@azimuth.greet(/g' \
    -e 's/assert_eq("\([^"]*\)", @azimuth\.greet("\([^"]*\)")/assert_eq_string("\1", @azimuth.greet("\2"))/g' \
    "$file" > "$temp_file"

# 替换原文件
mv "$temp_file" "$file"

echo "Fixed additional_edge_cases.mbt"