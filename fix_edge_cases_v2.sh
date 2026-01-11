#!/bin/bash

# 修复 additional_edge_cases.mbt 文件的所有语法错误
file="/home/runner/work/Azimuth/Azimuth/src/azimuth/test/additional_edge_cases.mbt"

# 创建临时文件
temp_file=$(mktemp)

# 读取文件内容并修复问题
sed -e 's/@azimuth\.add([^,]*, [^{]*{/@azimuth.add(\1, \2) {/g' \
    -e 's/@azimuth\.multiply([^,]*, [^{]*{/@azimuth.multiply(\1, \2) {/g' \
    -e 's/@azimuth\.greet([^)]*{/@azimuth.greet(\1) {/g' \
    -e 's/if "Hello != \([^"]*\)!", @azimuth\.greet("\([^"]*\)" {/if "Hello, \1!" != @azimuth.greet("\2") {/g' \
    "$file" > "$temp_file"

# 替换原文件
mv "$temp_file" "$file"

echo "Fixed additional_edge_cases.mbt syntax errors"