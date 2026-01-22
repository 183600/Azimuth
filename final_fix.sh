#!/bin/bash

# 最终修复脚本，解决类型不匹配和语法错误

echo "开始最终修复..."

# 修复类型不匹配问题
files=$(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" -exec grep -l "assert_eq.*>.*true\|assert_eq.*<.*true\|assert_eq.*>.*false\|assert_eq.*<.*false" {} \;)

count=0
for file in $files; do
    echo "修复类型不匹配: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 修复类型不匹配：调换参数顺序
    sed -e 's/@azimuth\.assert_eq(\([^>]*\) > \([^,]*\), true)/@azimuth.assert_eq(true, \1 > \2)/g' \
        -e 's/@azimuth\.assert_eq(\([^<]*\) < \([^,]*\), true)/@azimuth.assert_eq(true, \1 < \2)/g' \
        -e 's/@azimuth\.assert_eq(\([^>]*\) > \([^,]*\), false)/@azimuth.assert_eq(false, \1 > \2)/g' \
        -e 's/@azimuth\.assert_eq(\([^<]*\) < \([^,]*\), false)/@azimuth.assert_eq(false, \1 < \2)/g' \
        "$file" > "$temp_file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
    
    count=$((count + 1))
done

echo "修复了 $count 个类型不匹配问题"

# 修复保留关键字问题
files_use=$(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" -exec grep -l "^use " {} \;)

for file in $files_use; do
    echo "修复保留关键字: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 将use关键字替换为其他内容或删除
    sed 's/^use.*$/\/\/ use line removed/g' "$file" > "$temp_file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
done

echo "所有修复完成！"