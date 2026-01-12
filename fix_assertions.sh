#!/bin/bash

# 修复所有测试文件中的断言问题
echo "Fixing assertion issues in all test files..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
cd "$AZIMUTH_TEST_PATH"

# 替换 @azimuth.assert_eq 为 if 语句
for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "simple_test.mbt" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 使用 sed 替换断言
    sed -E 's/@azimuth\.assert_eq\(([^,]+),\s*([^)]+)\)/if \2 != \1 { @test.fail("Expected " + \1.to_string() + " but got " + \2.to_string()) }/g' "$file" > "$temp_file"
    
    # 替换 @azimuth.assert_eq_string
    sed -E -i 's/@azimuth\.assert_eq_string\(([^,]+),\s*([^)]+)\)/if \2 != \1 { @test.fail("Expected \"" + \1 + "\" but got \"" + \2 + "\"") }/g' "$temp_file"
    
    # 替换 @azimuth.assert_true
    sed -E -i 's/@azimuth\.assert_true\(([^)]+)\)/if !\1 { @test.fail("Expected true but got false") }/g' "$temp_file"
    
    # 替换 @azimuth.assert_false
    sed -E -i 's/@azimuth\.assert_false\(([^)]+)\)/if \1 { @test.fail("Expected false but got true") }/g' "$temp_file"
    
    # 移动临时文件到原文件
    mv "$temp_file" "$file"
  fi
done

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
cd "$CLEAN_TEST_PATH"

# 替换 @clean_test.assert_eq 为 if 语句
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 使用 sed 替换断言
    sed -E 's/@clean_test\.assert_eq\(([^,]+),\s*([^)]+)\)/if \2 != \1 { @test.fail("Expected " + \1.to_string() + " but got " + \2.to_string()) }/g' "$file" > "$temp_file"
    
    # 替换 @clean_test.assert_eq_string
    sed -E -i 's/@clean_test\.assert_eq_string\(([^,]+),\s*([^)]+)\)/if \2 != \1 { @test.fail("Expected \"" + \1 + "\" but got \"" + \2 + "\"") }/g' "$temp_file"
    
    # 替换 @clean_test.assert_true
    sed -E -i 's/@clean_test\.assert_true\(([^)]+)\)/if !\1 { @test.fail("Expected true but got false") }/g' "$temp_file"
    
    # 替换 @clean_test.assert_false
    sed -E -i 's/@clean_test\.assert_false\(([^)]+)\)/if \1 { @test.fail("Expected false but got true") }/g' "$temp_file"
    
    # 移动临时文件到原文件
    mv "$temp_file" "$file"
  fi
done

echo "All assertion fixes completed!"