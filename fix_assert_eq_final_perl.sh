#!/bin/bash

# 最终修复 assert_eq 函数调用的脚本

echo "开始修复所有 assert_eq 调用..."

# 使用 Perl 进行更精确的文本替换
find /home/runner/work/Azimuth/Azimuth/src -name "*.mbt" -exec grep -l "assert_eq" {} \; | while read file; do
    echo "修复文件: $file"
    
    # 使用 Perl 进行替换，它比 sed 更适合处理复杂的文本模式
    perl -i -pe '
        # 处理 assert_eq(expected, actual) 模式
        s/assert_eq\(\s*([^,]+?)\s*,\s*([^)]+?)\s*\)/if $1 != $2 { \@test.fail("Test failed") }/g;
        
        # 处理包含字符串的 assert_eq
        s/assert_eq\(\s*("([^"]*)")\s*,\s*("([^"]*)")\s*\)/if $1 != $3 { \@test.fail("Test failed") }/g;
        
        # 处理包含函数调用的 assert_eq
        s/assert_eq\(\s*(@[a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_]*\([^)]*\))\s*,\s*(@[a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_]*\([^)]*\))\s*\)/if $1 != $2 { \@test.fail("Test failed") }/g;
        
        # 处理变量和函数调用的组合
        s/assert_eq\(\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*,\s*(@[a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_]*\([^)]*\))\s*\)/if $1 != $2 { \@test.fail("Test failed") }/g;
        s/assert_eq\(\s*(@[a-zA-Z_][a-zA-Z0-9_]*\.[a-zA-Z_][a-zA-Z0-9_]*\([^)]*\))\s*,\s*([a-zA-Z_][a-zA-Z0-9_]*)\s*\)/if $1 != $2 { \@test.fail("Test failed") }/g;
    ' "$file"
done

echo "assert_eq 修复完成"