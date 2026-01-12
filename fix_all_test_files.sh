#!/bin/bash

# 批量修复测试文件的脚本
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 函数：修复单个测试文件
fix_test_file() {
  local file_path="$1"
  local pkg_name="$2"
  
  echo "Fixing $file_path..."
  
  # 创建临时文件
  local temp_file=$(mktemp)
  
  # 读取文件内容并修复
  while IFS= read -r line; do
    # 跳过 use 语句
    if [[ "$line" =~ ^use[[:space:]]+ ]]; then
      continue
    fi
    
    # 替换 assert_eq 调用
    if [[ "$line" =~ assert_eq\(([^,]+),\s*([^)]+)\) ]]; then
      local expected="${BASH_REMATCH[1]}"
      local actual="${BASH_REMATCH[2]}"
      echo "  let result = $actual" >> "$temp_file"
      echo "  if result != $expected {" >> "$temp_file"
      echo "    @builtin.panic()" >> "$temp_file"
      echo "  }" >> "$temp_file"
      continue
    fi
    
    # 替换 assert_eq_string 调用
    if [[ "$line" =~ assert_eq_string\(([^,]+),\s*([^)]+)\) ]]; then
      local expected="${BASH_REMATCH[1]}"
      local actual="${BASH_REMATCH[2]}"
      echo "  let result = $actual" >> "$temp_file"
      echo "  if result != $expected {" >> "$temp_file"
      echo "    @builtin.panic()" >> "$temp_file"
      echo "  }" >> "$temp_file"
      continue
    fi
    
    # 替换 assert_true 调用
    if [[ "$line" =~ assert_true\(([^)]+)\) ]]; then
      local condition="${BASH_REMATCH[1]}"
      echo "  if !($condition) {" >> "$temp_file"
      echo "    @builtin.panic()" >> "$temp_file"
      echo "  }" >> "$temp_file"
      continue
    fi
    
    # 替换 assert_false 调用
    if [[ "$line" =~ assert_false\(([^)]+)\) ]]; then
      local condition="${BASH_REMATCH[1]}"
      echo "  if $condition {" >> "$temp_file"
      echo "    @builtin.panic()" >> "$temp_file"
      echo "  }" >> "$temp_file"
      continue
    fi
    
    # 其他行保持不变
    echo "$line" >> "$temp_file"
  done < "$file_path"
  
  # 替换原文件
  mv "$temp_file" "$file_path"
}

# 修复 azimuth 测试文件
echo "=== Fixing azimuth test files ==="
cd "$AZIMUTH_PATH/test"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    # 跳过已经修复的文件
    if [[ "$file" == "simple_test_new.mbt" || "$file" == "test_functions.mbt" || "$file" == "test_helper.mbt" ]]; then
      echo "Skipping $file (already fixed)"
      continue
    fi
    
    fix_test_file "$file" "azimuth"
  fi
done

# 修复 clean_test 测试文件
echo ""
echo "=== Fixing clean_test test files ==="
cd "$CLEAN_TEST_PATH/test"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    # 跳过已经修复的文件
    if [[ "$file" == "test_functions.mbt" || "$file" == "test_helper.mbt" ]]; then
      echo "Skipping $file (already fixed)"
      continue
    fi
    
    fix_test_file "$file" "clean_test"
  fi
done

echo ""
echo "All test files have been fixed."