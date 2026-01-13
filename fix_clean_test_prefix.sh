#!/bin/bash

# 批量修复 clean_test 包中测试文件的函数调用，添加正确的包前缀
# 这个脚本会在函数调用前添加 clean_test:: 前缀

# 要添加前缀的函数列表
FUNCTIONS_TO_PREFIX=(
  "add("
  "multiply("
  "greet("
  "assert_eq("
  "assert_eq_string("
  "assert_true("
  "assert_false("
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
  
  # 逐行处理文件
  while IFS= read -r line; do
    modified_line="$line"
    
    # 为每个函数添加前缀
    for func in "${FUNCTIONS_TO_PREFIX[@]}"; do
      # 使用 sed 替换函数调用，添加 clean_test:: 前缀
      # 只替换不在行首且前面没有 clean_test:: 的情况
      modified_line=$(echo "$modified_line" | sed 's/\([^a-zA-Z_]\)\('"${func%/}"'\)/\1clean_test::\2/g')
    done
    
    echo "$modified_line" >> "$temp_file"
  done < "$test_file"
  
  # 替换原文件
  mv "$temp_file" "$test_file"
done

echo "All clean_test files have been processed with clean_test prefix."