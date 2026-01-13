#!/bin/bash

# 批量修复所有测试文件的导入问题
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 函数定义模板
FUNCTIONS_TEMPLATE='// 测试辅助函数定义
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

# 修复 azimuth 测试文件
echo "=== 修复 azimuth 测试文件 ==="
cd "$AZIMUTH_PATH/test"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "simple_test.mbt" ]; then
    echo "修复文件: $file"
    
    # 检查文件是否已经包含函数定义
    if ! grep -q "fn add(a : Int, b : Int) -> Int" "$file"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 写入函数定义
      echo "$FUNCTIONS_TEMPLATE" > "$temp_file"
      
      # 追加原文件内容（跳过注释和空行）
      awk '
        BEGIN { skip_comments = 1 }
        /^\/\/ 测试辅助函数定义/ { skip_comments = 0 }
        /^\/\/ 从/ { skip_comments = 0 }
        /^test / { skip_comments = 0 }
        !skip_comments && !/^\/\/ 测试辅助函数定义/ && !/^\/\/ 从/ && !/^$/ && !/^\/\/ 测试文件/ && !/^\/\/ 从 azimuth 包中复制/ { print }
      ' "$file" >> "$temp_file"
      
      # 替换原文件
      mv "$temp_file" "$file"
    fi
  fi
done

# 修复 clean_test 测试文件
echo ""
echo "=== 修复 clean_test 测试文件 ==="
cd "$CLEAN_TEST_PATH/test"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "simple_test.mbt" ]; then
    echo "修复文件: $file"
    
    # 检查文件是否已经包含函数定义
    if ! grep -q "fn add(a : Int, b : Int) -> Int" "$file"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 写入函数定义
      echo "$FUNCTIONS_TEMPLATE" > "$temp_file"
      
      # 追加原文件内容（跳过注释和空行）
      awk '
        BEGIN { skip_comments = 1 }
        /^\/\/ 测试辅助函数定义/ { skip_comments = 0 }
        /^\/\/ 从/ { skip_comments = 0 }
        /^test / { skip_comments = 0 }
        !skip_comments && !/^\/\/ 测试辅助函数定义/ && !/^\/\/ 从/ && !/^$/ && !/^\/\/ 测试文件/ && !/^\/\/ 从 azimuth 包中复制/ { print }
      ' "$file" >> "$temp_file"
      
      # 替换原文件
      mv "$temp_file" "$file"
    fi
  fi
done

echo ""
echo "所有测试文件修复完成！"