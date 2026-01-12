#!/bin/bash

# 修复测试文件中的函数调用问题
# 使用正确的导入语法

echo "开始修复测试文件中的函数调用问题..."

# 修复azimuth测试文件
echo "修复azimuth测试文件..."
find src/azimuth/test -name "*.mbt" -type f | while read file; do
  # 跳过test_helper.mbt
  if [[ "$file" == *"test_helper.mbt" ]]; then
    continue
  fi
  
  echo "修复文件: $file"
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 添加导入语句并修复函数调用
  {
    echo "// 测试函数和断言函数从 azimuth 包导入"
    echo ""
    # 处理文件内容，将函数调用替换为不带前缀的形式
    sed -e 's/azimuth::assert_eq(/assert_eq(/g' \
        -e 's/azimuth::assert_eq_string(/assert_eq_string(/g' \
        -e 's/azimuth::assert_true(/assert_true(/g' \
        -e 's/azimuth::assert_false(/assert_false(/g' \
        -e 's/azimuth::add(/add(/g' \
        -e 's/azimuth::multiply(/multiply(/g' \
        -e 's/azimuth::greet(/greet(/g' \
        "$file"
  } > "$temp_file"
  
  # 替换原文件
  mv "$temp_file" "$file"
done

# 修复clean_test测试文件
echo "修复clean_test测试文件..."
find src/clean_test/test -name "*.mbt" -type f | while read file; do
  # 跳过test_helper.mbt
  if [[ "$file" == *"test_helper.mbt" ]]; then
    continue
  fi
  
  echo "修复文件: $file"
  
  # 创建临时文件
  temp_file=$(mktemp)
  
  # 添加导入语句并修复函数调用
  {
    echo "// 测试函数和断言函数从 clean_test 包导入"
    echo ""
    # 处理文件内容，将函数调用替换为不带前缀的形式
    sed -e 's/test_helper::assert_eq(/assert_eq(/g' \
        -e 's/test_helper::assert_eq_string(/assert_eq_string(/g' \
        -e 's/test_helper::assert_true(/assert_true(/g' \
        -e 's/test_helper::assert_false(/assert_false(/g' \
        -e 's/test_helper::add(/add(/g' \
        -e 's/test_helper::multiply(/multiply(/g' \
        -e 's/test_helper::greet(/greet(/g' \
        "$file"
  } > "$temp_file"
  
  # 替换原文件
  mv "$temp_file" "$file"
done

echo "测试文件修复完成！"