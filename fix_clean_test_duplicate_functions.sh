#!/bin/bash

# 批量修复 clean_test 包中测试文件的重复函数定义问题
# 这个脚本会删除测试文件中与 clean_test 包中重复的函数定义

# 要删除的函数列表
FUNCTIONS_TO_REMOVE=(
  "fn add("
  "fn multiply("
  "fn greet("
  "fn assert_eq("
  "fn assert_eq_string("
  "fn assert_true("
  "fn assert_false("
)

# 测试文件目录
TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 处理每个测试文件
for test_file in "$TEST_DIR"/*.mbt; do
  # 跳过 moon.pkg.json 和其他非 .mbt 文件
  if [[ "$test_file" != *.mbt ]]; then
    continue
  fi
  
  echo "Processing $test_file..."
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 标记是否在函数定义中
  in_function=false
  function_start_line=0
  brace_count=0
  
  # 逐行处理文件
  line_num=0
  while IFS= read -r line; do
    line_num=$((line_num + 1))
    
    # 检查是否是要删除的函数定义行
    should_remove=false
    for func in "${FUNCTIONS_TO_REMOVE[@]}"; do
      if [[ "$line" == *"$func"* ]]; then
        should_remove=true
        in_function=true
        function_start_line=$line_num
        brace_count=0
        echo "  Found function to remove: $func at line $line_num"
        break
      fi
    done
    
    # 如果不在要删除的函数中，直接写入文件
    if [ "$in_function" = false ]; then
      echo "$line" >> "$temp_file"
    else
      # 在要删除的函数中，计算大括号数量以确定函数结束位置
      # 计算当前行中的开括号和闭括号数量
      open_braces=$(echo "$line" | grep -o '{' | wc -l)
      close_braces=$(echo "$line" | grep -o '}' | wc -l)
      brace_count=$((brace_count + open_braces - close_braces))
      
      # 如果大括号数量回到0或负数，说明函数结束
      if [ $brace_count -le 0 ]; then
        in_function=false
        echo "  Function ended at line $line_num"
      fi
    fi
  done < "$test_file"
  
  # 替换原文件
  mv "$temp_file" "$test_file"
done

echo "All clean_test files have been processed."