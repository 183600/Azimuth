#!/bin/bash

# 更精确地修复assert_true和assert_false函数的脚本

echo "开始精确修复assert_true和assert_false函数..."

# 查找所有包含assert_true或assert_false的.mbt文件
files=$(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" -exec grep -l "assert_eq(1, 1)(" {} \;)

count=0
for file in $files; do
    echo "处理文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 更精确的sed替换，修复错误的语法
    sed -e 's/assert_eq(1, 1)(\([^)]*\))/assert_eq(\1, true)/g' \
        -e 's/assert_eq(0, 0)(\([^)]*\))/assert_eq(\1, false)/g' \
        -e 's/@azimuth\.assert_eq(1, 1)(\([^)]*\))/@azimuth.assert_eq(\1, true)/g' \
        -e 's/@azimuth\.assert_eq(0, 0)(\([^)]*\))/@azimuth.assert_eq(\1, false)/g' \
        "$file" > "$temp_file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
    
    count=$((count + 1))
done

echo "修复完成！共处理了 $count 个文件。"

# 现在处理没有@azimuth前缀的文件，添加前缀
echo "处理缺少@azimuth前缀的文件..."

files_no_prefix=$(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" -exec grep -l "assert_eq.*add\|assert_eq.*multiply\|assert_eq.*greet" {} \; | xargs grep -L "@azimuth\.")

for file in $files_no_prefix; do
    echo "为文件添加前缀: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 为azimuth函数添加前缀
    sed -e 's/assert_eq(/@azimuth.assert_eq(/g' \
        -e 's/assert_eq_string(/@azimuth.assert_eq_string(/g' \
        -e 's/add(/@azimuth.add(/g' \
        -e 's/multiply(/@azimuth.multiply(/g' \
        -e 's/greet(/@azimuth.greet(/g' \
        -e 's/divide_with_ceil(/@azimuth.divide_with_ceil(/g' \
        "$file" > "$temp_file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
    
    count=$((count + 1))
done

echo "所有修复完成！"