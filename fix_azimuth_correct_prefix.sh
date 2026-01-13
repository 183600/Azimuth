#!/bin/bash

# 批量修复测试文件，使用正确的包名调用函数
# 这个脚本会在函数调用前添加正确的包前缀

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
TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

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
    
    # 为每个函数添加前缀，先移除错误的前缀
    modified_line=$(echo "$modified_line" | sed 's/azimuth:://g')
    
    # 为每个函数添加正确的前缀
    for func in "${FUNCTIONS_TO_PREFIX[@]}"; do
      # 使用 sed 替换函数调用，添加正确的包前缀
      modified_line=$(echo "$modified_line" | sed 's/\('"${func%/}"'\)/azimuth.\1/g')
    done
    
    echo "$modified_line" >> "$temp_file"
  done < "$test_file"
  
  # 替换原文件
  mv "$temp_file" "$test_file"
done

echo "All test files have been processed with correct azimuth prefix."