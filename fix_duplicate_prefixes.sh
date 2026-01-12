#!/bin/bash

# 修复重复的 clean_test:: 前缀

echo "Fixing duplicate clean_test:: prefixes..."

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
    
    # 修复重复的前缀：clean_test::clean_test:: -> clean_test::
    sed -i 's/clean_test::clean_test::/clean_test::/g' "$file"
    
    echo "Fixed $file"
  fi
done

echo "All duplicate prefixes have been fixed."