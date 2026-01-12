#!/bin/bash

# 修复双重前缀问题

echo "开始修复双重前缀问题..."

# 修复azimuth测试文件中的双重前缀
find src/azimuth/test -name "*.mbt" -type f | while read file; do
  echo "修复文件: $file"
  sed -i 's/azimuth::azimuth::/azimuth::/g' "$file"
done

# 修复clean_test测试文件中的双重前缀
find src/clean_test/test -name "*.mbt" -type f | while read file; do
  echo "修复文件: $file"
  sed -i 's/test_helper::test_helper::/test_helper::/g' "$file"
done

echo "双重前缀问题修复完成！"