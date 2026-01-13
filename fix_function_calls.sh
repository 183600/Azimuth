#!/bin/bash

# 修复测试文件中的函数调用问题

echo "Fixing function calls in test files..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 替换 @azimuth.add 为 add
    sed 's/@azimuth\.add(/add(/g' "$file" > "$temp_file"
    
    # 替换 @azimuth.multiply 为 multiply
    sed -i 's/@azimuth\.multiply(/multiply(/g' "$temp_file"
    
    # 替换 @azimuth.greet 为 greet
    sed -i 's/@azimuth\.greet(/greet(/g' "$temp_file"
    
    # 替换 @azimuth.assert_eq 为 assert_eq
    sed -i 's/@azimuth\.assert_eq(/assert_eq(/g' "$temp_file"
    
    # 替换 @azimuth.assert_eq_string 为 assert_eq_string
    sed -i 's/@azimuth\.assert_eq_string(/assert_eq_string(/g' "$temp_file"
    
    # 替换 @azimuth.assert_true 为 assert_true
    sed -i 's/@azimuth\.assert_true(/assert_true(/g' "$temp_file"
    
    # 替换 @azimuth.assert_false 为 assert_false
    sed -i 's/@azimuth\.assert_false(/assert_false(/g' "$temp_file"
    
    # 替换 @clean_test.add 为 add
    sed -i 's/@clean_test\.add(/add(/g' "$temp_file"
    
    # 替换 @clean_test.multiply 为 multiply
    sed -i 's/@clean_test\.multiply(/multiply(/g' "$temp_file"
    
    # 替换 @clean_test.greet 为 greet
    sed -i 's/@clean_test\.greet(/greet(/g' "$temp_file"
    
    # 替换 @clean_test.assert_eq 为 assert_eq
    sed -i 's/@clean_test\.assert_eq(/assert_eq(/g' "$temp_file"
    
    # 替换 @clean_test.assert_eq_string 为 assert_eq_string
    sed -i 's/@clean_test\.assert_eq_string(/assert_eq_string(/g' "$temp_file"
    
    # 替换 @clean_test.assert_true 为 assert_true
    sed -i 's/@clean_test\.assert_true(/assert_true(/g' "$temp_file"
    
    # 替换 @clean_test.assert_false 为 assert_false
    sed -i 's/@clean_test\.assert_false(/assert_false(/g' "$temp_file"
    
    # 替换 @builtin.panic() 为 panic()
    sed -i 's/@builtin\.panic()/panic()/g' "$temp_file"
    
    # 检查是否有变化
    if ! cmp -s "$file" "$temp_file"; then
      # 备份原文件
      cp "$file" "$file.bak"
      # 替换原文件
      mv "$temp_file" "$file"
      echo "  Fixed $file"
    else
      rm "$temp_file"
      echo "  No changes needed for $file"
    fi
  fi
done

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
cd "$CLEAN_TEST_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 替换 @azimuth.add 为 add
    sed 's/@azimuth\.add(/add(/g' "$file" > "$temp_file"
    
    # 替换 @azimuth.multiply 为 multiply
    sed -i 's/@azimuth\.multiply(/multiply(/g' "$temp_file"
    
    # 替换 @azimuth.greet 为 greet
    sed -i 's/@azimuth\.greet(/greet(/g' "$temp_file"
    
    # 替换 @azimuth.assert_eq 为 assert_eq
    sed -i 's/@azimuth\.assert_eq(/assert_eq(/g' "$temp_file"
    
    # 替换 @azimuth.assert_eq_string 为 assert_eq_string
    sed -i 's/@azimuth\.assert_eq_string(/assert_eq_string(/g' "$temp_file"
    
    # 替换 @azimuth.assert_true 为 assert_true
    sed -i 's/@azimuth\.assert_true(/assert_true(/g' "$temp_file"
    
    # 替换 @azimuth.assert_false 为 assert_false
    sed -i 's/@azimuth\.assert_false(/assert_false(/g' "$temp_file"
    
    # 替换 @clean_test.add 为 add
    sed -i 's/@clean_test\.add(/add(/g' "$temp_file"
    
    # 替换 @clean_test.multiply 为 multiply
    sed -i 's/@clean_test\.multiply(/multiply(/g' "$temp_file"
    
    # 替换 @clean_test.greet 为 greet
    sed -i 's/@clean_test\.greet(/greet(/g' "$temp_file"
    
    # 替换 @clean_test.assert_eq 为 assert_eq
    sed -i 's/@clean_test\.assert_eq(/assert_eq(/g' "$temp_file"
    
    # 替换 @clean_test.assert_eq_string 为 assert_eq_string
    sed -i 's/@clean_test\.assert_eq_string(/assert_eq_string(/g' "$temp_file"
    
    # 替换 @clean_test.assert_true 为 assert_true
    sed -i 's/@clean_test\.assert_true(/assert_true(/g' "$temp_file"
    
    # 替换 @clean_test.assert_false 为 assert_false
    sed -i 's/@clean_test\.assert_false(/assert_false(/g' "$temp_file"
    
    # 替换 @builtin.panic() 为 panic()
    sed -i 's/@builtin\.panic()/panic()/g' "$temp_file"
    
    # 检查是否有变化
    if ! cmp -s "$file" "$temp_file"; then
      # 备份原文件
      cp "$file" "$file.bak"
      # 替换原文件
      mv "$temp_file" "$file"
      echo "  Fixed $file"
    else
      rm "$temp_file"
      echo "  No changes needed for $file"
    fi
  fi
done

echo "Function call fixes complete."