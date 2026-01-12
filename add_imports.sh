#!/bin/bash

# 在测试文件中添加导入语句
echo "Adding imports to test files..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 修复 azimuth 测试文件
echo "Adding imports to azimuth test files..."
cd "$AZIMUTH_PATH/test"

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 检查是否已经有导入语句
    if ! grep -q "test" "$file" | grep -q "import"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 添加导入语句
      echo "test {" > "$temp_file"
      echo "  @azimuth.lib" >> "$temp_file"
      echo "}" >> "$temp_file"
      echo "" >> "$temp_file"
      
      # 添加原始内容
      cat "$file" >> "$temp_file"
      
      # 移动临时文件到原位置
      mv "$temp_file" "$file"
    fi
  fi
done

# 修复 clean_test 测试文件
echo "Adding imports to clean_test test files..."
cd "$CLEAN_TEST_PATH/test"

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 检查是否已经有导入语句
    if ! grep -q "test" "$file" | grep -q "import"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 添加导入语句
      echo "test {" > "$temp_file"
      echo "  @clean_test.lib" >> "$temp_file"
      echo "}" >> "$temp_file"
      echo "" >> "$temp_file"
      
      # 添加原始内容
      cat "$file" >> "$temp_file"
      
      # 移动临时文件到原位置
      mv "$temp_file" "$file"
    fi
  fi
done

echo "Imports added to test files!"