#!/bin/bash

# 修复测试文件 - 使用直接函数调用

echo "Fixing test files to use direct function calls..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 函数：修复测试文件
fix_test_file() {
  local file="$1"
  
  echo "Processing $file..."
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 修复函数调用 - 移除包前缀
  sed 's/azimuth\.add(/add(/g' "$file" > "$temp_file"
  sed -i 's/azimuth\.multiply(/multiply(/g' "$temp_file"
  sed -i 's/azimuth\.greet(/greet(/g' "$temp_file"
  sed -i 's/azimuth\.assert_eq(/assert_eq(/g' "$temp_file"
  sed -i 's/azimuth\.assert_eq_string(/assert_eq_string(/g' "$temp_file"
  sed -i 's/azimuth\.assert_true(/assert_true(/g' "$temp_file"
  sed -i 's/azimuth\.assert_false(/assert_false(/g' "$temp_file"
  
  sed -i 's/clean_test\.add(/add(/g' "$temp_file"
  sed -i 's/clean_test\.multiply(/multiply(/g' "$temp_file"
  sed -i 's/clean_test\.greet(/greet(/g' "$temp_file"
  sed -i 's/clean_test\.assert_eq(/assert_eq(/g' "$temp_file"
  sed -i 's/clean_test\.assert_eq_string(/assert_eq_string(/g' "$temp_file"
  sed -i 's/clean_test\.assert_true(/assert_true(/g' "$temp_file"
  sed -i 's/clean_test\.assert_false(/assert_false(/g' "$temp_file"
  
  # 修复 panic 调用
  sed -i 's/@builtin\.panic()/panic()/g' "$temp_file"
  
  # 检查是否有变化
  if ! cmp -s "$file" "$temp_file"; then
    # 备份原文件
    cp "$file" "$file.bak3"
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
    
    fix_test_file "$file"
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
    
    fix_test_file "$file"
  fi
done

echo "Direct function call fixes complete."