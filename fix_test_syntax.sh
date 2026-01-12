#!/bin/bash

# 修复测试文件中的函数调用语法
echo "Fixing test files..."

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
    
    # 替换 @azimuth.add() 为 add()
    sed 's/@azimuth\.add(/add(/g' "$file" > "$temp_file"
    
    # 替换 @azimuth.multiply() 为 multiply()
    sed -i 's/@azimuth\.multiply(/multiply(/g' "$temp_file"
    
    # 替换 @azimuth.greet() 为 greet()
    sed -i 's/@azimuth\.greet(/greet(/g' "$temp_file"
    
    # 替换 @test.fail() 为 raise @moonbitlang/core/builtin.Failure()
    sed -i 's/@test\.fail("\([^"]*\)")/raise @moonbitlang\/core\/builtin.Failure("\1")/g' "$temp_file"
    
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
    
    # 替换 @clean_test.add() 为 add()
    sed 's/@clean_test\.add(/add(/g' "$file" > "$temp_file"
    
    # 替换 @clean_test.multiply() 为 multiply()
    sed -i 's/@clean_test\.multiply(/multiply(/g' "$temp_file"
    
    # 替换 @clean_test.greet() 为 greet()
    sed -i 's/@clean_test\.greet(/greet(/g' "$temp_file"
    
    # 替换 @test.fail() 为 raise @moonbitlang/core/builtin.Failure()
    sed -i 's/@test\.fail("\([^"]*\)")/raise @moonbitlang\/core\/builtin.Failure("\1")/g' "$temp_file"
    
    # 移动临时文件到原位置
    mv "$temp_file" "$file"
  fi
done

echo "Test files fixed!"