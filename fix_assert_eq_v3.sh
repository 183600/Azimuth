#!/bin/bash

# 修复 assert_eq 函数调用的脚本 - 改进版

# 查找所有包含 assert_eq 的 .mbt 文件
find /home/runner/work/Azimuth/Azimuth/src -name "*.mbt" -exec grep -l "assert_eq" {} \; | while read file; do
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 使用更复杂的 sed 脚本来处理 assert_eq 调用
    # 首先处理简单的 assert_eq 调用
    sed -E 's/assert_eq\(([^,()]+),\s*([^,()]+)\)/if \1 != \2 { @test.fail("Test failed") }/g' "$file" > "$temp_file"
    
    # 处理包含函数调用的 assert_eq
    sed -i -E 's/assert_eq\((@azimuth\.[^,()]+),\s*(@azimuth\.[^,()]+)\)/if \1 != \2 { @test.fail("Test failed") }/g' "$temp_file"
    
    # 处理更复杂的情况
    sed -i -E 's/assert_eq\((@azimuth\.[^,]+),\s*([^)]+)\)/if \1 != \2 { @test.fail("Test failed") }/g' "$temp_file"
    sed -i -E 's/assert_eq\(([^,]+),\s*(@azimuth\.[^)]+)\)/if \1 != \2 { @test.fail("Test failed") }/g' "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
done

echo "assert_eq 修复完成"