#!/bin/bash

# 全面修复测试文件中的函数调用语法
echo "Comprehensively fixing test files..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
cd "$AZIMUTH_PATH/test"

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理整个文件
    while IFS= read -r line; do
      # 替换 @azimuth.add() 为 add()
      line=$(echo "$line" | sed 's/@azimuth\.add(/add(/g')
      
      # 替换 @azimuth.multiply() 为 multiply()
      line=$(echo "$line" | sed 's/@azimuth\.multiply(/multiply(/g')
      
      # 替换 @azimuth.greet() 为 greet()
      line=$(echo "$line" | sed 's/@azimuth\.greet(/greet(/g')
      
      # 替换 @test.fail() 为 raise @moonbitlang/core/builtin.Failure()
      line=$(echo "$line" | sed 's/@test\.fail("\([^"]*\)")/raise @moonbitlang\/core\/builtin.Failure("\1")/g')
      
      echo "$line" >> "$temp_file"
    done < "$file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
  fi
done

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
cd "$CLEAN_TEST_PATH/test"

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 处理整个文件
    while IFS= read -r line; do
      # 替换 @clean_test.add() 为 add()
      line=$(echo "$line" | sed 's/@clean_test\.add(/add(/g')
      
      # 替换 @clean_test.multiply() 为 multiply()
      line=$(echo "$line" | sed 's/@clean_test\.multiply(/multiply(/g')
      
      # 替换 @clean_test.greet() 为 greet()
      line=$(echo "$line" | sed 's/@clean_test\.greet(/greet(/g')
      
      # 替换 @test.fail() 为 raise @moonbitlang/core/builtin.Failure()
      line=$(echo "$line" | sed 's/@test\.fail("\([^"]*\)")/raise @moonbitlang\/core\/builtin.Failure("\1")/g')
      
      echo "$line" >> "$temp_file"
    done < "$file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
  fi
done

echo "Test files comprehensively fixed!"