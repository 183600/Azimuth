#!/bin/bash

# 批量修复所有测试文件，在文件开头添加函数定义
echo "Adding function definitions to all test files..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 函数定义模板
FUNCTION_DEFINITIONS='// 在测试文件中直接定义需要的函数

fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

'

# 查找所有 .mbt 文件
find "$TEST_DIR" -name "*.mbt" -type f | while read file; do
  # 检查文件是否已经包含函数定义
  if ! grep -q "fn add(a : Int, b : Int) -> Int" "$file"; then
    echo "Processing $file..."
    # 创建临时文件
    temp_file=$(mktemp)
    # 在文件开头添加函数定义
    echo "$FUNCTION_DEFINITIONS" > "$temp_file"
    cat "$file" >> "$temp_file"
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "Done adding function definitions."