#!/bin/bash

# 移除测试文件中的包前缀，因为我们现在使用 use 语句
# 这个脚本会移除函数调用前的 azimuth:: 或 clean_test:: 前缀

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
    # 移除 azimuth:: 前缀
    modified_line=$(echo "$line" | sed 's/azimuth:://g')
    echo "$modified_line" >> "$temp_file"
  done < "$test_file"
  
  # 替换原文件
  mv "$temp_file" "$test_file"
done

echo "All test files have been processed to remove azimuth prefix."