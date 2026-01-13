#!/bin/bash

# 批量修复测试文件的脚本
echo "批量修复测试文件..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 需要修复的测试文件列表
AZIMUTH_TEST_FILES=(
    "additional_comprehensive_test.mbt"
    "additional_tests.mbt"
    "all_tests.mbt"
    "basic_test.mbt"
    "enhanced_tests.mbt"
    "math_fundamentals_test.mbt"
    "new_tests.mbt"
)

CLEAN_TEST_FILES=(
    # 目前clean_test只有一个测试文件，已经修复
)

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

# 修复azimuth测试文件
echo "修复azimuth测试文件..."
cd "$AZIMUTH_TEST_PATH"

for file in "${AZIMUTH_TEST_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "修复 $file..."
        
        # 读取原文件内容
        original_content=$(cat "$file")
        
        # 创建新文件内容
        new_content="${FUNCTIONS_TEMPLATE}${original_content}"
        
        # 写入新内容
        echo "$new_content" > "$file"
        
        echo "已修复 $file"
    else
        echo "文件 $file 不存在，跳过"
    fi
done

echo "azimuth测试文件修复完成"

# 修复clean_test测试文件
echo "修复clean_test测试文件..."
cd "$CLEAN_TEST_PATH"

for file in "${CLEAN_TEST_FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "修复 $file..."
        
        # 读取原文件内容
        original_content=$(cat "$file")
        
        # 创建新文件内容
        new_content="${FUNCTIONS_TEMPLATE}${original_content}"
        
        # 写入新内容
        echo "$new_content" > "$file"
        
        echo "已修复 $file"
    else
        echo "文件 $file 不存在，跳过"
    fi
done

echo "clean_test测试文件修复完成"
echo "所有测试文件修复完成"