#!/bin/bash

# 清理 clean_test 中的重复函数定义
echo "Cleaning duplicate function definitions in clean_test..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 移除断言函数定义（保留在第一个文件中）
find "$TEST_DIR" -name "*.mbt" -type f | while read file; do
  # 跳过第一个文件
  FIRST_FILE=$(find "$TEST_DIR" -name "*.mbt" -type f | head -1)
  if [ "$file" != "$FIRST_FILE" ]; then
    # 检查文件是否包含断言函数定义
    if grep -q "fn assert_eq" "$file"; then
      echo "Removing assertion function definitions from $file..."
      # 创建临时文件，移除断言函数定义
      temp_file=$(mktemp)
      # 移除断言函数定义
      sed '/^fn assert_eq(expected : Int, actual : Int) -> Unit {/,/^}/d' "$file" > "$temp_file"
      sed -i '/^fn assert_eq_string(expected : String, actual : String) -> Unit {/,/^}/d' "$temp_file"
      sed -i '/^fn assert_true(condition : Bool) -> Unit {/,/^}/d' "$temp_file"
      sed -i '/^fn assert_false(condition : Bool) -> Unit {/,/^}/d' "$temp_file"
      # 替换原文件
      mv "$temp_file" "$file"
    fi
  fi
done

echo "Done cleaning duplicate function definitions."