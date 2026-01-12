#!/bin/bash

# 批量添加导入语句到测试文件

echo "批量添加导入语句到测试文件..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "添加导入语句到 azimuth 测试文件..."
cd "$AZIMUTH_TEST_PATH"

# 遍历所有 .mbt 文件
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "处理 $file..."
    
    # 检查是否已经包含导入语句
    if ! grep -q "^import" "$file"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 添加导入语句
      echo "import \"../lib\"" > "$temp_file"
      echo "" >> "$temp_file"
      
      # 添加原文件内容
      cat "$file" >> "$temp_file"
      
      # 替换原文件
      mv "$temp_file" "$file"
    fi
  fi
done

# 修复 clean_test 测试文件
echo "添加导入语句到 clean_test 测试文件..."
cd "$CLEAN_TEST_PATH"

# 遍历所有 .mbt 文件
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "处理 $file..."
    
    # 检查是否已经包含导入语句
    if ! grep -q "^import" "$file"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 添加导入语句
      echo "import \"../lib\"" > "$temp_file"
      echo "" >> "$temp_file"
      
      # 添加原文件内容
      cat "$file" >> "$temp_file"
      
      # 替换原文件
      mv "$temp_file" "$file"
    fi
  fi
done

echo "所有测试文件导入语句添加完成！"