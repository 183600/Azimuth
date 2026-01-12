#!/bin/bash

# 修复测试文件中的包引用问题

echo "Fixing package references in test files..."

# 修复 azimuth 测试文件中的包引用
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 替换 @azimuth. 为空（因为函数在同一包中）
    sed -i 's/@azimuth\.//g' "$file"
    
    # 确保导入了正确的包
    if ! grep -q "import" "$file"; then
      # 如果没有导入语句，添加在文件开头
      sed -i '1i\test' "$file"
    fi
  fi
done

echo "Fixed azimuth test files"

# 修复 clean_test 测试文件中的包引用
cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 替换 @clean_test. 为空（因为函数在同一包中）
    sed -i 's/@clean_test\.//g' "$file"
    
    # 确保导入了正确的包
    if ! grep -q "import" "$file"; then
      # 如果没有导入语句，添加在文件开头
      sed -i '1i\test' "$file"
    fi
  fi
done

echo "Fixed clean_test test files"

echo "All package references fixed"