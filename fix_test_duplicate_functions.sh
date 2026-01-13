#!/bin/bash

# 批量修复测试文件中的重复函数定义问题

# 测试文件目录
TEST_DIR="src/azimuth/test"

# 需要删除的重复函数列表
FUNCTIONS_TO_REMOVE=("add" "multiply" "greet" "assert_eq" "assert_eq_string" "assert_true" "assert_false")

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
    # 跳过非测试文件
    if [[ ! -f "$test_file" ]]; then
        continue
    fi
    
    # 跳过已修复的文件
    if grep -q "使用 azimuth 包中的函数" "$test_file"; then
        echo "跳过已修复的文件: $test_file"
        continue
    fi
    
    echo "正在修复文件: $test_file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 标记是否在函数定义区域
    in_function_section=false
    function_start_pattern=""
    
    # 处理文件内容
    while IFS= read -r line; do
        # 检查是否是函数定义开始
        if [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+(add|multiply|greet|assert_eq|assert_eq_string|assert_true|assert_false)[[:space:]]*\(.*\)[[:space:]]*->[[:space:]]*.*$ ]]; then
            in_function_section=true
            function_start_pattern="${BASH_REMATCH[1]}"
            continue
        fi
        
        # 如果在函数定义区域，检查是否是函数定义结束
        if [[ "$in_function_section" == true ]]; then
            # 检查是否是测试开始或空行（表示函数定义结束）
            if [[ "$line" =~ ^[[:space:]]*test[[:space:]]+.* ]] || [[ "$line" =~ ^[[:space:]]*$ ]]; then
                in_function_section=false
                # 如果这是测试开始，输出测试替换注释
                if [[ "$line" =~ ^[[:space:]]*test[[:space:]]+.* ]]; then
                    echo "// 使用 azimuth 包中的函数" >> "$temp_file"
                fi
                echo "$line" >> "$temp_file"
            fi
            # 跳过函数定义内容
            continue
        fi
        
        # 如果不在函数定义区域，直接输出
        if [[ "$in_function_section" == false ]]; then
            echo "$line" >> "$temp_file"
        fi
    done < "$test_file"
    
    # 替换原文件
    mv "$temp_file" "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有测试文件修复完成！"