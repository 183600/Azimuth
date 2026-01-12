#!/bin/bash

# 更准确地修复测试文件中缺少的结束大括号

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
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
      echo "修复文件: $file - 添加 $missing_braces 个结束大括号"
      echo "函数数量: $func_count, 测试块数量: $test_count, 当前大括号: $brace_count, 需要: $needed_braces"
      
      # 添加缺少的大括号
      for ((i=1; i<=missing_braces; i++)); do
        echo "}" >> "$file"
      done
    fi
  fi
done

echo "所有测试文件的结束大括号已修复完成"