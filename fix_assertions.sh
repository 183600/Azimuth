#!/bin/bash

# 修复所有测试文件中的断言函数

cd /home/runner/work/Azimuth/Azimuth

# 查找所有测试文件
TEST_FILES=$(find src -name "*.mbt")

for file in $TEST_FILES; do
  # 检查文件是否包含断言函数
  if grep -q "azimuth::assert_eq" "$file" || grep -q "azimuth::assert_eq_string" "$file" || grep -q "azimuth::assert_true" "$file" || grep -q "azimuth::assert_false" "$file"; then
    echo "Fixing assertions in $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理文件内容，替换断言函数
    awk '
    /azimuth::assert_eq/ {
      # 找到assert_eq调用
      match($0, /azimuth::assert_eq\(([^,]+),\s*([^)]+)\)/, arr)
      if (arr[1] != "" && arr[2] != "") {
        # 替换为if语句
        gsub(/azimuth::assert_eq\(([^,]+),\s*([^)]+)\)/, "if \1 != \2 {\n    @builtin.panic()\n  }")
      }
      print
      next
    }
    /azimuth::assert_eq_string/ {
      # 找到assert_eq_string调用
      match($0, /azimuth::assert_eq_string\(([^,]+),\s*([^)]+)\)/, arr)
      if (arr[1] != "" && arr[2] != "") {
        # 替换为if语句
        gsub(/azimuth::assert_eq_string\(([^,]+),\s*([^)]+)\)/, "if \1 != \2 {\n    @builtin.panic()\n  }")
      }
      print
      next
    }
    /azimuth::assert_true/ {
      # 找到assert_true调用
      match($0, /azimuth::assert_true\(([^)]+)\)/, arr)
      if (arr[1] != "") {
        # 替换为if语句
        gsub(/azimuth::assert_true\(([^)]+)\)/, "if !\1 {\n    @builtin.panic()\n  }")
      }
      print
      next
    }
    /azimuth::assert_false/ {
      # 找到assert_false调用
      match($0, /azimuth::assert_false\(([^)]+)\)/, arr)
      if (arr[1] != "") {
        # 替换为if语句
        gsub(/azimuth::assert_false\(([^)]+)\)/, "if \1 {\n    @builtin.panic()\n  }")
      }
      print
      next
    }
    {
      print
    }
    ' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "Fixed assertions in all test files"