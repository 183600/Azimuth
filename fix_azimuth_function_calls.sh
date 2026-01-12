#!/bin/bash

# 批量修复azimuth测试文件中的函数调用
echo "Fixing azimuth function calls in test files..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 查找所有测试文件
find "$TEST_DIR" -name "*.mbt" -type f | while read file; do
  # 跳过已经修复的文件
  if grep -q "azimuth.azimuth\." "$file"; then
    echo "Skipping $file (already fixed)"
    continue
  fi
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 替换函数调用
  sed -e 's/azimuth\.add(/azimuth.azimuth.add(/g' \
      -e 's/azimuth\.multiply(/azimuth.azimuth.multiply(/g' \
      -e 's/azimuth\.greet(/azimuth.azimuth.greet(/g' \
      -e 's/azimuth\.assert_eq(/azimuth.azimuth.assert_eq(/g' \
      -e 's/azimuth\.assert_eq_string(/azimuth.azimuth.assert_eq_string(/g' \
      -e 's/azimuth\.assert_true(/azimuth.azimuth.assert_true(/g' \
      -e 's/azimuth\.assert_false(/azimuth.azimuth.assert_false(/g' \
      "$file" > "$temp_file"
  
  # 检查是否有变化
  if ! cmp -s "$file" "$temp_file"; then
    echo "Fixed $file"
    mv "$temp_file" "$file"
  else
    rm "$temp_file"
  fi
done

echo "Fix completed!"