#!/bin/bash

# 批量修复测试文件，使用 using 关键字导入包
# 这个脚本会在测试文件开头添加 using azimuth; 语句

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
  
  # 检查文件是否已经有 using 语句
  has_using=false
  while IFS= read -r line; do
    if [[ "$line" == "using azimuth"* ]]; then
      has_using=true
      break
    fi
  done < "$test_file"
  
  # 如果没有 using 语句，添加它
  if [ "$has_using" = false ]; then
    # 移除 use 语句并添加 using 语句
    grep -v "use azimuth;" "$test_file" > "$temp_file"
    echo "using azimuth;" | cat - "$temp_file" > "$test_file"
  fi
  
  rm "$temp_file"
done

echo "All test files have been processed with using azimuth statement."