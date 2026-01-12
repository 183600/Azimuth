#!/bin/bash

# 脚本用于删除测试文件中的重复函数定义

cd src/clean_test/test

# 需要删除的函数定义模式
PATTERNS_TO_REMOVE=(
  "^fn add(a : Int, b : Int) -> Int {"
  "^fn multiply(a : Int, b : Int) -> Int {"
  "^fn greet(name : String) -> String {"
)

# 处理每个测试文件（除了test_helper.mbt）
for file in *.mbt; do
  if [ "$file" != "test_helper.mbt" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 删除函数定义块
    awk '
    /^fn add\(a : Int, b : Int\) -> Int \{/ {
      in_add_func = 1
      next
    }
    /^fn multiply\(a : Int, b : Int\) -> Int \{/ {
      in_multiply_func = 1
      next
    }
    /^fn greet\(name : String\) -> String \{/ {
      in_greet_func = 1
      next
    }
    
    # 如果在函数中，检查是否到达函数结束
    in_add_func || in_multiply_func || in_greet_func {
      if (/^}/) {
        in_add_func = 0
        in_multiply_func = 0
        in_greet_func = 0
      }
      next
    }
    
    # 其他行正常输出
    {
      print
    }
    ' "$file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
  fi
done

echo "Done removing duplicate function definitions."