#!/bin/bash

# 批量修复azimuth测试文件中的导入问题
TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

echo "Fixing import issues in azimuth test files..."

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
  if [ -f "$test_file" ]; then
    echo "Processing $(basename "$test_file")..."
    
    # 检查文件是否包含重复定义的函数
    if grep -q "^fn add(" "$test_file"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 移除重复定义的函数，保留测试部分
      awk '
      /^test / { in_test = 1 }
      in_test { print }
      /^fn / { in_test = 0 }
      ' "$test_file" > "$temp_file"
      
      # 修复函数调用，添加azimuth::前缀
      sed -i "s/add(/azimuth::add(/g" "$temp_file"
      sed -i "s/multiply(/azimuth::multiply(/g" "$temp_file"
      sed -i "s/greet(/azimuth::greet(/g" "$temp_file"
      sed -i "s/assert_eq(/azimuth::assert_eq(/g" "$temp_file"
      sed -i "s/assert_eq_string(/azimuth::assert_eq_string(/g" "$temp_file"
      sed -i "s/assert_true(/azimuth::assert_true(/g" "$temp_file"
      sed -i "s/assert_false(/azimuth::assert_false(/g" "$temp_file"
      
      # 添加注释
      echo "// 测试文件 - 使用azimuth包中的函数" > "$test_file"
      cat "$temp_file" >> "$test_file"
      
      # 清理临时文件
      rm "$temp_file"
    fi
  fi
done

echo "Fixed import issues in azimuth test files."