#!/bin/bash

# 修复测试文件中的其他语法错误
echo "开始修复测试文件中的其他语法错误..."

# 查找所有需要修复的文件
files=$(find src/azimuth/test -name "*.mbt" -type f)

for file in $files; do
    echo "修复文件: $file"
    # 修复 }) -> }
    sed -i 's/})/}/g' "$file"
    
    # 修复 "Hello != World!", greet("World") { -> "Hello, World!" != greet("World") {
    sed -i 's/"Hello != World!", greet("World") {/"Hello, World!" != greet("World") {/g' "$file"
    
    # 修复其他可能的字符串比较错误
    sed -i 's/"Hello != Alice!", greet("Alice") {/"Hello, Alice!" != greet("Alice") {/g' "$file"
    sed -i 's/"Hello != Bob!", greet("Bob") {/"Hello, Bob!" != greet("Bob") {/g' "$file"
done

echo "修复完成！"