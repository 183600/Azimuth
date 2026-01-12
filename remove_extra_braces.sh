#!/bin/bash

# 删除测试文件中多余的结束大括号

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "检查文件: $file"
    
    # 统计函数定义数量
    func_count=$(grep -c "^pub fn " "$file")
    
    # 统计测试块数量
    test_count=$(grep -c "^test " "$file")
    
    # 统计所有结束大括号数量
    brace_count=$(grep -c "^}" "$file")
    
    # 计算需要的结束大括号数量
    needed_braces=$((func_count + test_count))
    
    # 如果结束大括号数量过多，删除多余的大括号
    if [ "$brace_count" -gt "$needed_braces" ]; then
      excess_braces=$((brace_count - needed_braces))
      echo "删除 $excess_braces 个多余的结束大括号从 $file"
      
      # 删除多余的大括号（从文件末尾开始）
      for ((i=1; i<=excess_braces; i++)); do
        # 删除文件末尾的空行和大括号
        sed -i '/^$/d' "$file"  # 删除空行
        sed -i '/^}$/d' "$file"  # 删除大括号行
      done
    fi
  fi
done

echo "所有测试文件的多余大括号已删除完成"