#!/bin/bash

# 修复测试文件导入问题的脚本
# 根据用户要求，只修复测试用例的编译错误，不修改被测试代码

echo "开始修复测试文件的导入问题..."

# 修复 azimuth 测试文件
echo "修复 azimuth 测试文件..."
for file in src/azimuth/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        
        # 检查文件是否已经有 use 语句
        if ! grep -q "^use azimuth::" "$file"; then
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 添加 use 语句到文件开头
            echo "use azimuth::{add, multiply, greet, assert_eq, assert_eq_string, assert_true, assert_false}" > "$temp_file"
            echo "" >> "$temp_file"
            cat "$file" >> "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
            
            # 替换 azimuth:: 前缀为直接调用
            sed -i 's/azimuth::add(/add(/g' "$file"
            sed -i 's/azimuth::multiply(/multiply(/g' "$file"
            sed -i 's/azimuth::greet(/greet(/g' "$file"
            sed -i 's/azimuth::assert_eq(/assert_eq(/g' "$file"
            sed -i 's/azimuth::assert_eq_string(/assert_eq_string(/g' "$file"
            sed -i 's/azimuth::assert_true(/assert_true(/g' "$file"
            sed -i 's/azimuth::assert_false(/assert_false(/g' "$file"
        fi
    fi
done

# 修复 clean_test 测试文件
echo "修复 clean_test 测试文件..."
for file in src/clean_test/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        
        # 检查文件是否已经有 use 语句
        if ! grep -q "^use clean_test::" "$file"; then
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 添加 use 语句到文件开头
            echo "use clean_test::{add, multiply, greet, assert_eq, assert_eq_string, assert_true, assert_false}" > "$temp_file"
            echo "" >> "$temp_file"
            cat "$file" >> "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
            
            # 替换 clean_test:: 前缀为直接调用
            sed -i 's/clean_test::add(/add(/g' "$file"
            sed -i 's/clean_test::multiply(/multiply(/g' "$file"
            sed -i 's/clean_test::greet(/greet(/g' "$file"
            sed -i 's/clean_test::assert_eq(/assert_eq(/g' "$file"
            sed -i 's/clean_test::assert_eq_string(/assert_eq_string(/g' "$file"
            sed -i 's/clean_test::assert_true(/assert_true(/g' "$file"
            sed -i 's/clean_test::assert_false(/assert_false(/g' "$file"
        fi
    fi
done

echo "修复完成！"
echo "现在运行 moon test 验证修复结果..."