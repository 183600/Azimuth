#!/bin/bash

# 批量移除所有测试文件中的 use 语句
echo "Removing 'use' statements from test files..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 查找所有包含 use 语句的 .mbt 文件
find "$TEST_DIR" -name "*.mbt" -type f | while read file; do
  # 检查文件是否包含 use 语句
  if grep -q "^use " "$file"; then
    echo "Processing $file..."
    # 创建临时文件
    temp_file=$(mktemp)
    # 移除所有以 use 开头的行
    sed '/^use /d' "$file" > "$temp_file"
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "Done removing 'use' statements."