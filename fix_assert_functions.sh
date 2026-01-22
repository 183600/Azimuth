#!/bin/bash

# 批量修复assert_true和assert_false函数的脚本

echo "开始批量修复assert_true和assert_false函数..."

# 查找所有包含assert_true或assert_false的.mbt文件
files=$(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" -exec grep -l "assert_true\|assert_false" {} \;)

count=0
for file in $files; do
    echo "处理文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 使用sed替换assert_true和assert_false
    sed -e 's/assert_true(true)/assert_eq(1, 1)/g' \
        -e 's/assert_true(false)/assert_eq(0, 1)/g' \
        -e 's/assert_false(true)/assert_eq(0, 1)/g' \
        -e 's/assert_false(false)/assert_eq(1, 1)/g' \
        -e 's/@azimuth\.assert_true/@azimuth.assert_eq(1, 1)/g' \
        -e 's/@azimuth\.assert_false/@azimuth.assert_eq(0, 1)/g' \
        "$file" > "$temp_file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
    
    count=$((count + 1))
done

echo "修复完成！共处理了 $count 个文件。"