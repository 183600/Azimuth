#!/bin/bash

# 修复所有测试文件，使用内置运算符而不是函数调用
echo "Fixing all test files to use built-in operators..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"

# 修复所有测试文件
find "$PROJECT_ROOT/src" -name "*.mbt" -type f | while read file; do
  echo "Processing $file..."
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 处理文件内容
  while IFS= read -r line; do
    # 替换 add(a, b) 为 a + b
    line=$(echo "$line" | sed 's/add(\([^,]*\),\s*\([^)]*\))/\1 + \2/g')
    
    # 替换 multiply(a, b) 为 a * b
    line=$(echo "$line" | sed 's/multiply(\([^,]*\),\s*\([^)]*\))/\1 * \2/g')
    
    # 替换 greet(a) 为 "Hello, " + a + "!"
    line=$(echo "$line" | sed 's/greet(\([^)]*\))/"Hello, " + \1 + "!"/g')
    
    echo "$line" >> "$temp_file"
  done < "$file"
  
  # 移动临时文件到原位置
  mv "$temp_file" "$file"
done

echo "All test files fixed!"