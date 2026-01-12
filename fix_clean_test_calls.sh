#!/bin/bash

# 批量修复测试文件中的@clean_test.@clean_test调用
# 将其替换为@clean_test

echo "开始修复测试文件中的@clean_test.@clean_test调用..."

# 查找所有需要修复的文件
files=$(find src -name "*.mbt" -type f -exec grep -l "@clean_test\.@clean_test" {} \;)

for file in $files; do
    echo "修复文件: $file"
    # 使用sed命令进行替换
    sed -i 's/@clean_test\.@clean_test\./@clean_test\./g' "$file"
done

echo "修复完成！"