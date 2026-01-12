#!/bin/bash

# 修复clean_test目录下所有测试文件的导入语句

cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

# 遍历所有测试文件
for file in *.mbt; do
    # 跳过已经正确的文件
    if [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
        # 检查文件是否已经包含use语句
        if ! grep -q "use clean_test" "$file"; then
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 添加use语句到文件开头
            echo "use clean_test::{add, multiply, greet, assert_eq, assert_eq_string, assert_true, assert_false}" > "$temp_file"
            echo "" >> "$temp_file"
            
            # 读取原文件内容，跳过空行
            awk 'NF {print}' "$file" >> "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
            
            # 替换@azimuth.add为add
            sed -i 's/@azimuth\.add(/add(/g' "$file"
            sed -i 's/@azimuth\.multiply(/multiply(/g' "$file"
            sed -i 's/@azimuth\.greet(/greet(/g' "$file"
            sed -i 's/@azimuth\.assert_eq(/assert_eq(/g' "$file"
            sed -i 's/@azimuth\.assert_eq_string(/assert_eq_string(/g' "$file"
            sed -i 's/@azimuth\.assert_true(/assert_true(/g' "$file"
            sed -i 's/@azimuth\.assert_false(/assert_false(/g' "$file"
            
            # 替换azimuth::add为add
            sed -i 's/azimuth::add(/add(/g' "$file"
            sed -i 's/azimuth::multiply(/multiply(/g' "$file"
            sed -i 's/azimuth::greet(/greet(/g' "$file"
            sed -i 's/azimuth::assert_eq(/assert_eq(/g' "$file"
            sed -i 's/azimuth::assert_eq_string(/assert_eq_string(/g' "$file"
            sed -i 's/azimuth::assert_true(/assert_true(/g' "$file"
            sed -i 's/azimuth::assert_false(/assert_false(/g' "$file"
            
            echo "Fixed $file"
        fi
    fi
done

echo "All clean_test files have been fixed!"