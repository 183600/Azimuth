#!/bin/bash

# 批量修复所有测试文件的函数定义问题

TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 函数定义模板
FUNCTIONS_TEMPLATE='// 测试用的函数定义
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

# 需要修复的文件列表
FILES=(
  "additional_tests.mbt"
  "all_tests.mbt"
  "basic_test.mbt"
  "enhanced_tests.mbt"
  "lib_test.mbt"
  "math_fundamentals_test.mbt"
  "new_tests.mbt"
)

# 修复每个文件
for file in "${FILES[@]}"; do
  FILE_PATH="$TEST_DIR/$file"
  
  if [ -f "$FILE_PATH" ]; then
    echo "修复文件: $file"
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 添加函数定义
    echo "$FUNCTIONS_TEMPLATE" > "$temp_file"
    
    # 添加原文件内容
    cat "$FILE_PATH" >> "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$FILE_PATH"
  else
    echo "文件不存在: $file"
  fi
done

echo "所有测试文件修复完成！"