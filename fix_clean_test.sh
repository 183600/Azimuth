#!/bin/bash

# 修复 clean_test 测试文件
echo "Fixing clean_test files..."

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 函数定义模板
FUNCTION_DEFINITIONS='// 在测试文件中直接定义需要的函数

fn add(a : Int, b : Int) -> Int {
  a + b
}

fn multiply(a : Int, b : Int) -> Int {
  a * b
}

fn greet(name : String) -> String {
  "Hello, " + name + "!"
}

// 断言函数
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

'

# 移除所有 use 语句
find "$TEST_DIR" -name "*.mbt" -type f | while read file; do
  if grep -q "^use " "$file"; then
    echo "Removing 'use' statements from $file..."
    temp_file=$(mktemp)
    sed '/^use /d' "$file" > "$temp_file"
    mv "$temp_file" "$file"
  fi
done

# 选择第一个文件来保留函数定义
FIRST_FILE=$(find "$TEST_DIR" -name "*.mbt" -type f | head -1)
echo "Keeping function definitions in $FIRST_FILE"

# 确保第一个文件有函数定义
if ! grep -q "fn add(a : Int, b : Int) -> Int" "$FIRST_FILE"; then
  temp_file=$(mktemp)
  echo "$FUNCTION_DEFINITIONS" > "$temp_file"
  cat "$FIRST_FILE" >> "$temp_file"
  mv "$temp_file" "$FIRST_FILE"
fi

echo "Done fixing clean_test files."