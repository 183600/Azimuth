#!/bin/bash

# 为每个测试文件添加函数定义
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 函数定义
FUNCTIONS='// 测试辅助函数定义
pub fn add(a : Int, b : Int) -> Int {
  a + b
}

pub fn multiply(a : Int, b : Int) -> Int {
  a * b
}

pub fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

pub fn assert_true(condition : Bool) -> Unit {
  if !condition {
    @builtin.panic()
  }
}

pub fn assert_false(condition : Bool) -> Unit {
  if condition {
    @builtin.panic()
  }
}

'

# 函数：修复单个文件
fix_file() {
  local file_path="$1"
  echo "修复文件: $file_path"
  
  # 创建临时文件
  local temp_file=$(mktemp)
  
  # 添加函数定义
  echo "$FUNCTIONS" > "$temp_file"
  
  # 添加原文件内容
  cat "$file_path" >> "$temp_file"
  
  # 替换原文件
  mv "$temp_file" "$file_path"
}

# 修复 azimuth 测试文件
echo "=== 修复 azimuth 测试文件 ==="
cd "$AZIMUTH_TEST_PATH"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    # 跳过已经包含函数定义的文件
    if [ "$file" != "test_shared.mbt" ] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
      # 检查文件是否已经包含函数定义
      if ! grep -q "pub fn add" "$file"; then
        fix_file "$file"
      fi
    fi
  fi
done

# 修复 clean_test 测试文件
echo ""
echo "=== 修复 clean_test 测试文件 ==="
cd "$CLEAN_TEST_PATH"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    # 检查文件是否已经包含函数定义
    if ! grep -q "fn add" "$file"; then
      fix_file "$file"
    fi
  fi
done

echo ""
echo "修复完成！"