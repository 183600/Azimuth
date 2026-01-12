#!/bin/bash

# 全面修复测试文件中的问题

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "处理文件: $file"
    
    # 检查文件是否以大括号结尾
    if [ "$(tail -c 1 "$file")" != "}" ]; then
      echo "文件不以大括号结尾，检查是否需要添加"
      
      # 统计函数定义数量
      func_count=$(grep -c "^pub fn " "$file")
      
      # 统计测试块数量
      test_count=$(grep -c "^test " "$file")
      
      # 统计所有结束大括号数量
      brace_count=$(grep -c "^}" "$file")
      
      # 计算需要的结束大括号数量
      needed_braces=$((func_count + test_count))
      
      # 如果结束大括号数量不足，添加缺少的大括号
      if [ "$brace_count" -lt "$needed_braces" ]; then
        missing_braces=$((needed_braces - brace_count))
        echo "添加 $missing_braces 个结束大括号到 $file"
        
        # 添加缺少的大括号
        for ((i=1; i<=missing_braces; i++)); do
          echo "}" >> "$file"
        done
      elif [ "$brace_count" -eq "$needed_braces" ]; then
        # 如果数量匹配但文件不以大括号结尾，可能最后缺少换行符
        echo "}" >> "$file"
      fi
    fi
  fi
done

echo "所有测试文件已处理完成"