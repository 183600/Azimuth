#!/bin/bash

# 修复 assert_eq 函数调用的脚本

# 查找所有包含 assert_eq 的 .mbt 文件
find /home/runner/work/Azimuth/Azimuth/src -name "*.mbt" -exec grep -l "assert_eq" {} \; | while read file; do
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 使用 sed 替换 assert_eq 调用
    # 模式: assert_eq(expected, actual) -> if expected != actual { @test.fail("Test failed") }
    sed -E 's/assert_eq\(([^,]+),\s*([^)]+)\)/if \1 != \2 { @test.fail("Test failed") }/g' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
done

echo "assert_eq 修复完成"