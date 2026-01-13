#!/bin/bash

# 为测试文件添加正确的包前缀

# 测试文件目录
TEST_DIR="src/azimuth/test"

# 需要添加前缀的函数列表
FUNCTIONS_TO_PREFIX=("add" "multiply" "greet" "assert_eq" "assert_eq_string" "assert_true" "assert_false")

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
    # 跳过非测试文件
    if [[ ! -f "$test_file" ]]; then
        continue
    fi
    
    echo "正在修复文件: $test_file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容
    while IFS= read -r line; do
        # 为每个函数添加前缀
        modified_line="$line"
        for func in "${FUNCTIONS_TO_PREFIX[@]}"; do
            # 使用 sed 替换函数调用，添加包前缀
            modified_line=$(echo "$modified_line" | sed "s/\b$func\(/azimuth.$func\(/g")
        done
        
        echo "$modified_line" >> "$temp_file"
    done < "$test_file"
    
    # 替换原文件
    mv "$temp_file" "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有测试文件前缀修复完成！"