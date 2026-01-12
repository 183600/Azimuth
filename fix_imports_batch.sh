#!/bin/bash

# 批量修复测试文件中的导入问题

echo "开始修复测试文件的导入问题..."

# 修复azimuth测试文件
echo "修复azimuth测试文件..."
for file in src/azimuth/test/*.mbt; do
    if [ -f "$file" ]; then
        # 检查文件是否包含assert_eq、add等函数但没有use语句
        if grep -q "assert_eq\|assert_eq_string\|assert_true\|assert_false\|add\|multiply\|greet" "$file" && ! grep -q "use azimuth" "$file"; then
            echo "修复文件: $file"
            # 在文件开头添加use语句
            sed -i '1i use azimuth::{add, multiply, greet, assert_eq, assert_eq_string, assert_true, assert_false}' "$file"
        fi
    fi
done

# 修复clean_test测试文件
echo "修复clean_test测试文件..."
for file in src/clean_test/test/*.mbt; do
    if [ -f "$file" ]; then
        # 检查文件是否包含assert_eq、add等函数但没有use语句
        if grep -q "assert_eq\|assert_eq_string\|assert_true\|assert_false\|add\|multiply\|greet" "$file" && ! grep -q "use clean_test" "$file"; then
            echo "修复文件: $file"
            # 在文件开头添加use语句
            sed -i '1i use clean_test::{add, multiply, greet, assert_eq, assert_eq_string, assert_true, assert_false}' "$file"
        fi
    fi
done

echo "修复完成！"