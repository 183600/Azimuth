#!/bin/bash

# 修复 assert_eq 函数调用的脚本 - 简化版

# 查找所有包含 assert_eq 的 .mbt 文件
find /home/runner/work/Azimuth/Azimuth/src -name "*.mbt" -exec grep -l "assert_eq" {} \; | while read file; do
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 首先恢复文件，因为之前的修复可能有问题
    git checkout "$file" 2>/dev/null || true
    
    # 使用更简单的方法处理 assert_eq
    # 先处理简单的 assert_eq 调用
    sed -E 's/assert_eq\(([0-9-]+),\s*([0-9-]+)\)/if \1 != \2 { @test.fail("Test failed") }/g' "$file" > "$temp_file"
    
    # 处理包含函数调用的 assert_eq
    sed -i -E 's/assert_eq\((@[a-zA-Z_]+\.[a-zA-Z_]+\([^)]+\)),\s*(@[a-zA-Z_]+\.[a-zA-Z_]+\([^)]+\))\)/if \1 != \2 { @test.fail("Test failed") }/g' "$temp_file"
    
    # 处理包含字符串的 assert_eq
    sed -i -E 's/assert_eq\(("[^"]+"),\s*("[^"]+")\)/if \1 != \2 { @test.fail("Test failed") }/g' "$temp_file"
    
    # 处理包含变量和函数调用的 assert_eq
    sed -i -E 's/assert_eq\(([a-zA-Z_][a-zA-Z0-9_]*),\s*(@[a-zA-Z_]+\.[a-zA-Z_]+\([^)]+\))\)/if \1 != \2 { @test.fail("Test failed") }/g' "$temp_file"
    sed -i -E 's/assert_eq\((@[a-zA-Z_]+\.[a-zA-Z_]+\([^)]+\)),\s*([a-zA-Z_][a-zA-Z0-9_]*)\)/if \1 != \2 { @test.fail("Test failed") }/g' "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
done

echo "assert_eq 修复完成"