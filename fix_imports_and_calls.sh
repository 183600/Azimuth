#!/bin/bash

# 修复测试文件中的所有导入和函数调用问题

echo "Fixing imports and function calls in test files..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 函数：修复测试文件
fix_test_file() {
  local file="$1"
  local pkg_name="$2"
  
  echo "Processing $file..."
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 首先检查是否需要添加导入
  if ! grep -q "test-import" "$file" && ! grep -q "import" "$file"; then
    # 添加导入语句在文件开头
    echo "test \"${pkg_name}_init\" { }" > "$temp_file"
    echo "" >> "$temp_file"
    cat "$file" >> "$temp_file"
    mv "$temp_file" "$file"
    temp_file=$(mktemp)
  fi
  
  # 修复函数调用
  sed "s/add(/${pkg_name}.add(/g" "$file" > "$temp_file"
  sed -i "s/multiply(/${pkg_name}.multiply(/g" "$temp_file"
  sed -i "s/greet(/${pkg_name}.greet(/g" "$temp_file"
  sed -i "s/assert_eq(/${pkg_name}.assert_eq(/g" "$temp_file"
  sed -i "s/assert_eq_string(/${pkg_name}.assert_eq_string(/g" "$temp_file"
  sed -i "s/assert_true(/${pkg_name}.assert_true(/g" "$temp_file"
  sed -i "s/assert_false(/${pkg_name}.assert_false(/g" "$temp_file"
  
  # 修复 panic 调用
  sed -i 's/panic()/@builtin.panic()/g' "$temp_file"
  
  # 检查是否有变化
  if ! cmp -s "$file" "$temp_file"; then
    # 备份原文件
    cp "$file" "$file.bak2"
    # 替换原文件
    mv "$temp_file" "$file"
    echo "  Fixed $file"
  else
    rm "$temp_file"
    echo "  No changes needed for $file"
  fi
}

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ]; then
    # 跳过某些文件
    if [[ "$file" == "test_functions.mbt" || "$file" == "test_functions_def.mbt" || "$file" == "test_helper.mbt" || "$file" == "test_shared.mbt" ]]; then
      echo "Skipping $file (helper file)"
      continue
    fi
    
    fix_test_file "$file" "azimuth"
  fi
done

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
cd "$CLEAN_TEST_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ]; then
    # 跳过某些文件
    if [[ "$file" == "test_helpers.mbt" ]]; then
      echo "Skipping $file (helper file)"
      continue
    fi
    
    fix_test_file "$file" "clean_test"
  fi
done

echo "Import and function call fixes complete."