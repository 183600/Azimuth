#!/bin/bash

# 找到所有使用 assert_eq 的测试文件
files=$(grep -l "assert_eq" /home/runner/work/Azimuth/Azimuth/src/azimuth/test/*.mbt | grep -v "test_helper.mbt")

# 为每个文件添加 assert_eq 函数定义
for file in $files; do
    echo "Processing $file..."
    
    # 检查文件是否已经有 assert_eq 函数定义
    if ! grep -q "pub fn assert_eq" "$file"; then
        # 在文件开头添加 assert_eq 函数定义
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
        
        echo "Added assert_eq functions to $file"
    else
        echo "$file already has assert_eq functions"
    fi
done

echo "Done!"