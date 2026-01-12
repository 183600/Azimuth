#!/bin/bash

# 修复测试文件中缺少的结束大括号

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    # 检查文件是否以大括号结尾
    if [ "$(tail -c 1 "$file")" != "}" ]; then
      echo "修复文件: $file - 添加结束大括号"
      
      # 检查是否有未闭合的测试块
      test_blocks=$(grep -c "^test " "$file")
      closing_braces=$(grep -c "^}" "$file")
      
      # 如果测试块数量大于闭合大括号数量，添加缺少的大括号
      if [ "$test_blocks" -gt "$closing_braces" ]; then
        missing_braces=$((test_blocks - closing_braces))
        echo "添加 $missing_braces 个结束大括号到 $file"
        
        # 添加缺少的大括号
        for ((i=1; i<=missing_braces; i++)); do
          echo "}" >> "$file"
        done
      fi
    fi
  fi
done

echo "所有测试文件的结束大括号已修复完成"