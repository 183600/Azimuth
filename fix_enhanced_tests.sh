#!/bin/bash

# 修复 additional_enhanced_tests.mbt 文件的语法错误
file="/home/runner/work/Azimuth/Azimuth/src/azimuth/test/additional_enhanced_tests.mbt"

# 创建临时文件
temp_file=$(mktemp)

# 读取文件内容并修复问题
sed -e 's/assert_eq_string("[^"]*", @azimuth\.greet("[^"]*")))/assert_eq_string("\1", @azimuth.greet("\2"))/g' \
    -e 's/assert_eq("[^"]*", @azimuth\.greet("[^"]*"))/assert_eq_string("\1", @azimuth.greet("\2"))/g' \
    "$file" > "$temp_file"

# 替换原文件
mv "$temp_file" "$file"

echo "Fixed additional_enhanced_tests.mbt syntax errors"