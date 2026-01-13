#!/bin/bash

# 批量修复测试文件，使用 use 关键字导入包
# 这个脚本会在测试文件开头添加 use azimuth; 语句

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
  
  # 检查文件是否已经有 use 语句
  has_use=false
  while IFS= read -r line; do
    if [[ "$line" == "use azimuth"* ]]; then
      has_use=true
      break
    fi
  done < "$test_file"
  
  # 如果没有 use 语句，添加它
  if [ "$has_use" = false ]; then
    echo "use azimuth;" > "$temp_file"
    cat "$test_file" >> "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$test_file"
  else
    rm "$temp_file"
  fi
done

echo "All test files have been processed with use azimuth statement."