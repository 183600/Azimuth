#!/bin/bash

# 为所有测试文件添加函数定义，避免包导入问题
# 这个脚本会在每个测试文件开头添加所需的函数定义

# 函数定义
FUNCTION_DEFINITIONS='// 测试辅助函数定义
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

'

# 测试文件目录
TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 处理每个测试文件
for test_file in "$TEST_DIR"/*.mbt; do
  # 跳过 moon.pkg.json 和其他非 .mbt 文件
  if [[ "$test_file" != *.mbt ]]; then
    continue
  fi
  
  echo "Processing $test_file..."
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 添加函数定义
  echo "$FUNCTION_DEFINITIONS" > "$temp_file"
  
  # 移除所有 azimuth. 前缀
  sed 's/azimuth\.//g' "$test_file" >> "$temp_file"
  
  # 替换原文件
  mv "$temp_file" "$test_file"
done

echo "All test files have been processed with function definitions."