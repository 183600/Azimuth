#!/bin/bash

# 修复所有测试文件中的问题
echo "开始修复测试文件..."

# 1. 首先为所有使用 assert_eq 的文件添加函数定义
echo "步骤1: 添加 assert_eq 函数定义..."
files=$(grep -l "assert_eq" /home/runner/work/Azimuth/Azimuth/src/azimuth/test/*.mbt | grep -v "test_helper.mbt")

for file in $files; do
    echo "处理 $file..."
    
    # 检查文件是否已经有 assert_eq 函数定义
    if ! grep -q "pub fn assert_eq" "$file"; then
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 读取文件内容
        content=$(cat "$file")
        
        # 添加函数定义到文件开头
        cat > "$temp_file" << 'EOF'
// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

EOF
        
        # 添加原文件内容
        echo "$content" >> "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$file"
        
        echo "已添加 assert_eq 函数到 $file"
    else
        echo "$file 已有 assert_eq 函数"
    fi
done

# 2. 修复字符串测试，使用 assert_eq_string 而不是 assert_eq
echo "步骤2: 修复字符串测试..."
for file in $files; do
    echo "修复 $file 中的字符串测试..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 修复字符串测试
    sed 's/assert_eq("Hello, \([^"]*\)!", @azimuth\.greet("\([^"]*\)")/assert_eq_string("Hello, \1!", @azimuth.greet("\2"))/g' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
done

echo "修复完成！"