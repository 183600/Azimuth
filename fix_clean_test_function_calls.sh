#!/bin/bash

# 批量修复 clean_test 测试文件中的函数调用

echo "Fixing function calls in clean_test test files..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 需要修复的函数列表
FUNCTIONS="add multiply greet assert_eq assert_eq_string assert_true assert_false"

# 遍历所有测试文件
for file in "$TEST_DIR"/*.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 跳过 test_helper.mbt
    if [[ "$file" == *"test_helper.mbt"* ]]; then
      echo "Skipping test_helper.mbt"
      continue
    fi
    
    # 为每个函数添加 clean_test:: 前缀
    for func in $FUNCTIONS; do
      # 使用 sed 替换函数调用，但避免替换已经带有前缀的调用
      sed -i "s/\b$func(/clean_test::$func(/g" "$file"
    done
    
    echo "Fixed $file"
  fi
done

echo "All test files have been fixed."