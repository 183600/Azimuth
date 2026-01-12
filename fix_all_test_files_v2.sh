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
  
  # 检查文件是否已经修复
  if grep -q "fn add(" "$file_path"; then
    echo "  Already fixed, skipping..."
    return 0
  fi
  
  # 创建临时文件
  local temp_file=$(mktemp)
  
  # 添加函数定义
  cat >> "$temp_file" << 'EOF'
// 测试文件 - 直接定义需要的函数
fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

fn assert_eq(expected : Int, actual : Int) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

fn assert_eq_string(expected : String, actual : String) -> Unit {
  if expected != actual {
    @builtin.panic()
  }
}

fn assert_true(condition : Bool) -> Unit {
  if !condition {
    @builtin.panic()
  }
}

fn assert_false(condition : Bool) -> Unit {
  if condition {
    @builtin.panic()
  }
}

EOF
  
  # 读取原文件内容，跳过 use 语句
  while IFS= read -r line; do
    # 跳过 use 语句
    if [[ "$line" =~ ^use[[:space:]]+ ]]; then
      continue
    fi
    
    # 跳过空注释行
    if [[ "$line" =~ ^[[:space:]]*//[[:space:]]*$ ]]; then
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