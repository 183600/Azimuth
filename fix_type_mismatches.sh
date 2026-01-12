#!/bin/bash

# 修复测试文件中的类型不匹配问题

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "检查文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容
    while IFS= read -r line; do
      # 修复字符串比较问题
      if echo "$line" | grep -q "assert_eq.*greet.*\""; then
        line=$(echo "$line" | sed 's/assert_eq(/assert_eq_string(/g')
      fi
      
      # 修复长度比较问题
      if echo "$line" | grep -q "assert_eq_string.*\.length()"; then
        line=$(echo "$line" | sed 's/assert_eq_string(/assert_eq(/g')
      fi
      
      echo "$line" >> "$temp_file"
    done < "$file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "所有测试文件的类型不匹配问题已修复完成"