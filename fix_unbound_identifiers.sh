#!/bin/bash

# 修复测试文件中未绑定的标识符问题

echo "开始修复测试文件中的未绑定标识符问题..."

# 定义要修复的目录
directories=(
    "src/azimuth/test"
    "src/clean_test/test"
    "src/test_only/test"
)

# 为每个目录修复文件
for dir in "${directories[@]}"; do
    if [ -d "$dir" ]; then
        echo "正在修复目录: $dir"
        
        # 查找所有.mbt文件（排除备份文件）
        find "$dir" -name "*.mbt" -not -name "*.bak*" | while read file; do
            echo "处理文件: $file"
            
            # 根据目录确定包前缀
            if [[ "$dir" == *"azimuth"* ]]; then
                prefix="@azimuth"
            elif [[ "$dir" == *"clean_test"* ]]; then
                prefix="@clean_test"
            elif [[ "$dir" == *"test_only"* ]]; then
                prefix="@test_only"
            fi
            
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 应用修复
            sed -E "
                # 替换函数调用，添加包前缀
                s/\bassert_eq\(/$prefix.assert_eq(/g
                s/\bassert_eq_string\(/$prefix.assert_eq_string(/g
                s/\bassert_true\(/$prefix.assert_true(/g
                s/\bassert_false\(/$prefix.assert_false(/g
                s/\badd\(/$prefix.add(/g
                s/\bmultiply\(/$prefix.multiply(/g
                s/\bgreet\(/$prefix.greet(/g
                
                # 替换@builtin.panic()为正确的断言
                s/@builtin\.panic()/$prefix.assert_true(false)/g
                
                # 确保已经正确使用前缀的不会重复添加
                s/$prefix\.$prefix\./$prefix./g
            " "$file" > "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
        done
    fi
done

echo "修复完成！"