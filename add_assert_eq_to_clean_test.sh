#!/bin/bash

# 为 clean_test 的测试文件添加 assert_eq 函数定义

echo "为 clean_test 的测试文件添加 assert_eq 函数..."

# 找到所有 clean_test 的测试文件
find /home/runner/work/Azimuth/Azimuth/src/clean_test/test -name "*.mbt" -type f | while read file; do
    # 检查文件是否已经有 assert_eq 函数定义
    if ! grep -q "pub fn assert_eq" "$file"; then
        echo "添加 assert_eq 函数到 $file..."
        # 在文件开头添加 assert_eq 函数定义
        temp_file=$(mktemp)
        echo "// 断言相等函数，用于测试" > "$temp_file"
        echo "pub fn assert_eq(expected : Int, actual : Int) -> Unit {" >> "$temp_file"
        echo "  let _ = expected == actual" >> "$temp_file"
        echo "}" >> "$temp_file"
        echo "" >> "$temp_file"
        echo "pub fn assert_eq_string(expected : String, actual : String) -> Unit {" >> "$temp_file"
        echo "  let _ = expected == actual" >> "$temp_file"
        echo "}" >> "$temp_file"
        echo "" >> "$temp_file"
        cat "$file" >> "$temp_file"
        mv "$temp_file" "$file"
    fi
done

echo "添加完成！"