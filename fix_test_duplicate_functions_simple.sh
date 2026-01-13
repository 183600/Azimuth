#!/bin/bash

# 批量修复测试文件中的重复函数定义问题

# 测试文件目录
TEST_DIR="src/azimuth/test"

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
    
    # 标记是否找到了函数定义
    found_functions=false
    
    # 处理文件内容
    while IFS= read -r line; do
        # 检查是否是函数定义开始
        if [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+add[[:space:]]*\( ]]; then
            found_functions=true
            continue
        elif [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+multiply[[:space:]]*\( ]]; then
            found_functions=true
            continue
        elif [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+greet[[:space:]]*\( ]]; then
            found_functions=true
            continue
        elif [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+assert_eq[[:space:]]*\( ]]; then
            found_functions=true
            continue
        elif [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+assert_eq_string[[:space:]]*\( ]]; then
            found_functions=true
            continue
        elif [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+assert_true[[:space:]]*\( ]]; then
            found_functions=true
            continue
        elif [[ "$line" =~ ^[[:space:]]*fn[[:space:]]+assert_false[[:space:]]*\( ]]; then
            found_functions=true
            continue
        fi
        
        # 如果找到了函数定义，检查是否是测试开始
        if [[ "$found_functions" == true ]] && [[ "$line" =~ ^[[:space:]]*test[[:space:]]+ ]]; then
            found_functions=false
            echo "// 使用 azimuth 包中的函数" >> "$temp_file"
            echo "$line" >> "$temp_file"
        # 如果没有在函数定义区域，直接输出
        elif [[ "$found_functions" == false ]]; then
            echo "$line" >> "$temp_file"
        fi
        # 如果在函数定义区域，跳过
    done < "$test_file"
    
    # 替换原文件
    mv "$temp_file" "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有测试文件修复完成！"