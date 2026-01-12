#!/bin/bash

# 修复测试文件中的函数调用和类型不匹配问题

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ] && grep -q "assert_eq.*greet" "$file"; then
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容
    while IFS= read -r line; do
      # 替换 assert_eq(..., greet(...)) 为 assert_eq_string(..., @azimuth.greet(...))
      if echo "$line" | grep -q "assert_eq.*greet"; then
        line=$(echo "$line" | sed 's/assert_eq(/assert_eq_string(/g')
        line=$(echo "$line" | sed 's/greet(/@azimuth.greet(/g')
      fi
      
      # 替换其他 greet 调用
      if echo "$line" | grep -q "greet(" && echo "$line" | grep -v "@azimuth.greet("; then
        line=$(echo "$line" | sed 's/greet(/@azimuth.greet(/g')
      fi
      
      # 替换其他 add 和 multiply 调用
      if echo "$line" | grep -q "add(" && echo "$line" | grep -v "@azimuth.add("; then
        line=$(echo "$line" | sed 's/add(/@azimuth.add(/g')
      fi
      
      if echo "$line" | grep -q "multiply(" && echo "$line" | grep -v "@azimuth.multiply("; then
        line=$(echo "$line" | sed 's/multiply(/@azimuth.multiply(/g')
      fi
      
      echo "$line" >> "$temp_file"
    done < "$file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "所有测试文件已修复完成"