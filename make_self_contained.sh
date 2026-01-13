#!/bin/bash

# 最终解决方案：将所有测试文件修改为自包含的形式

echo "Final solution: converting all test files to self-contained form..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 创建通用的函数定义
create_common_functions() {
  cat << 'EOF'
// 通用函数定义
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
}

# 修复 azimuth 测试文件
echo "Fixing azimuth test files..."
cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_functions_def.mbt" ] && [ "$file" != "test_helper.mbt" ] && [ "$file" != "test_shared.mbt" ] && [ "$file" != "test_helpers.mbt" ] && [ "$file" != "simple_self_contained.mbt" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 添加通用函数定义
    create_common_functions > "$temp_file"
    
    # 添加原文件内容（跳过空行）
    grep -v '^$' "$file" >> "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
    echo "  Fixed $file"
  fi
done

# 修复 clean_test 测试文件
echo "Fixing clean_test test files..."
cd "$CLEAN_TEST_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "test_helpers.mbt" ]; then
    echo "Processing $file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 添加通用函数定义
    create_common_functions > "$temp_file"
    
    # 添加原文件内容（跳过空行）
    grep -v '^$' "$file" >> "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$file"
    echo "  Fixed $file"
  fi
done

echo "Self-contained test files conversion complete."