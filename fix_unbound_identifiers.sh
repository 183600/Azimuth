#!/bin/bash

# 修复测试文件中未绑定标识符的错误
echo "Fixing unbound identifier errors in test files..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 函数：修复单个文件中的未绑定标识符错误
fix_unbound_identifiers() {
  local file_path="$1"
  local temp_file="${file_path}.tmp"
  local pkg_name="$2"
  
  # 检查文件是否存在
  if [ ! -f "$file_path" ]; then
    return 0
  fi
  
  # 跳过备份文件和日志文件
  if [[ "$file_path" =~ \.bak$ ]] || [[ "$file_path" =~ \.log$ ]]; then
    return 0
  fi
  
  # 创建临时文件
  cp "$file_path" "$temp_file"
  
  # 修复未绑定的标识符
  # 将 add 替换为 @pkg_name.add
  sed -i "s/\badd(/@${pkg_name}.add(/g" "$temp_file"
  # 将 multiply 替换为 @pkg_name.multiply
  sed -i "s/\bmultiply(/@${pkg_name}.multiply(/g" "$temp_file"
  # 将 greet 替换为 @pkg_name.greet
  sed -i "s/\bgreet(/@${pkg_name}.greet(/g" "$temp_file"
  # 将 assert_eq 替换为 @pkg_name.assert_eq
  sed -i "s/\bassert_eq(/@${pkg_name}.assert_eq(/g" "$temp_file"
  # 将 assert_eq_string 替换为 @pkg_name.assert_eq_string
  sed -i "s/\bassert_eq_string(/@${pkg_name}.assert_eq_string(/g" "$temp_file"
  # 将 assert_true 替换为 @pkg_name.assert_true
  sed -i "s/\bassert_true(/@${pkg_name}.assert_true(/g" "$temp_file"
  # 将 assert_false 替换为 @pkg_name.assert_false
  sed -i "s/\bassert_false(/@${pkg_name}.assert_false(/g" "$temp_file"
  
  # 如果文件有变化，则替换原文件
  if ! diff -q "$file_path" "$temp_file" > /dev/null 2>&1; then
    mv "$temp_file" "$file_path"
    echo "Fixed unbound identifiers in $file_path"
  else
    rm "$temp_file"
  fi
}

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
find "$AZIMUTH_TEST_PATH" -name "*.mbt" ! -path "*/.*" | while read file; do
  fix_unbound_identifiers "$file" "azimuth"
done

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
find "$CLEAN_TEST_PATH" -name "*.mbt" ! -path "*/.*" | while read file; do
  fix_unbound_identifiers "$file" "clean_test"
done

echo "Done fixing unbound identifier errors."