#!/bin/bash

# 修复未使用函数警告的脚本
echo "修复未使用函数警告..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 修复 azimuth 测试文件
echo "修复 azimuth 测试文件..."
cd "$AZIMUTH_PATH/test"

# 处理 backup 目录中的文件
for file in backup/*.mbt; do
  if [ -f "$file" ]; then
    # 检查文件是否包含未使用的断言函数
    if grep -q "assert_eq\|assert_eq_string\|assert_true\|assert_false" "$file"; then
      # 检查是否真的使用了这些函数
      if ! grep -q "@azimuth.assert_eq\|@azimuth.assert_eq_string\|@azimuth.assert_true\|@azimuth.assert_false" "$file"; then
        echo "处理文件: $file"
        # 移除未使用的断言函数定义
        sed -i '/^pub fn assert_eq/,/^}/d' "$file"
        sed -i '/^pub fn assert_eq_string/,/^}/d' "$file"
        sed -i '/^pub fn assert_true/,/^}/d' "$file"
        sed -i '/^pub fn assert_false/,/^}/d' "$file"
      fi
    fi
    
    # 移除未使用的功能函数
    if grep -q "fn add\|fn multiply\|fn greet" "$file"; then
      # 检查是否真的使用了这些函数
      if ! grep -q "@azimuth.add\|@azimuth.multiply\|@azimuth.greet" "$file"; then
        echo "移除未使用的功能函数: $file"
        sed -i '/^pub fn add/,/^}/d' "$file"
        sed -i '/^pub fn multiply/,/^}/d' "$file"
        sed -i '/^pub fn greet/,/^}/d' "$file"
      fi
    fi
  fi
done

# 修复 clean_test 测试文件
echo "修复 clean_test 测试文件..."
cd "$CLEAN_TEST_PATH/test"

# 处理 backup 目录中的文件
for file in backup/*.mbt; do
  if [ -f "$file" ]; then
    # 检查文件是否包含未使用的断言函数
    if grep -q "assert_eq\|assert_eq_string\|assert_true\|assert_false" "$file"; then
      # 检查是否真的使用了这些函数
      if ! grep -q "@clean_test.assert_eq\|@clean_test.assert_eq_string\|@clean_test.assert_true\|@clean_test.assert_false" "$file"; then
        echo "处理文件: $file"
        # 移除未使用的断言函数定义
        sed -i '/^pub fn assert_eq/,/^}/d' "$file"
        sed -i '/^pub fn assert_eq_string/,/^}/d' "$file"
        sed -i '/^pub fn assert_true/,/^}/d' "$file"
        sed -i '/^pub fn assert_false/,/^}/d' "$file"
      fi
    fi
    
    # 移除未使用的功能函数
    if grep -q "fn add\|fn multiply\|fn greet" "$file"; then
      # 检查是否真的使用了这些函数
      if ! grep -q "@clean_test.add\|@clean_test.multiply\|@clean_test.greet" "$file"; then
        echo "移除未使用的功能函数: $file"
        sed -i '/^pub fn add/,/^}/d' "$file"
        sed -i '/^pub fn multiply/,/^}/d' "$file"
        sed -i '/^pub fn greet/,/^}/d' "$file"
      fi
    fi
  fi
done

echo "修复完成！"