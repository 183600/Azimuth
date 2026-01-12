#!/bin/bash

# 修复测试文件中的括号不匹配问题
echo "开始修复测试文件中的语法错误..."

# 查找所有需要修复的文件
files=$(grep -l "add(2, 3 {" src/azimuth/test/*.mbt 2>/dev/null)

for file in $files; do
    echo "修复文件: $file"
    # 修复 add(2, 3 { -> add(2, 3) {
    sed -i 's/add(2, 3 {/add(2, 3) {/g' "$file"
    
    # 修复 multiply(2, 3 { -> multiply(2, 3) {
    sed -i 's/multiply(2, 3 {/multiply(2, 3) {/g' "$file"
    
    # 修复 greet("World" { -> greet("World") {
    sed -i 's/greet("World" {/greet("World") {/g' "$file"
    
    # 修复其他可能的括号问题
    sed -i 's/add(5, 7 {/add(5, 7) {/g' "$file"
    sed -i 's/multiply(4, 5 {/multiply(4, 5) {/g' "$file"
    sed -i 's/greet("Alice" {/greet("Alice") {/g' "$file"
    sed -i 's/greet("Bob" {/greet("Bob") {/g' "$file"
done

echo "修复完成！"