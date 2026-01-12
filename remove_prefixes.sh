#!/bin/bash

# 移除 clean_test:: 前缀，尝试直接使用函数名

echo "Removing clean_test:: prefixes..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 遍历所有测试文件
for file in "$TEST_DIR"/*.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 跳过 test_helper.mbt
    if [[ "$file" == *"test_helper.mbt"* ]]; then
      echo "Skipping test_helper.mbt"
      continue
    fi
    
    # 移除 clean_test:: 前缀
    sed -i 's/clean_test:://g' "$file"
    
    echo "Fixed $file"
  fi
done

echo "All clean_test:: prefixes have been removed."