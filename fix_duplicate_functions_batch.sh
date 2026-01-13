#!/bin/bash

# 批量修复测试文件中的重复函数定义
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 函数：修复单个文件
fix_file() {
  local file_path="$1"
  echo "修复文件: $file_path"
  
  # 创建临时文件
  local temp_file=$(mktemp)
  
  # 查找第一个 test 关键字的位置
  local test_line=$(grep -n "^test " "$file_path" | head -1 | cut -d: -f1)
  
  if [ ! -z "$test_line" ]; then
    # 保留从 test 开始的所有内容
    tail -n +$test_line "$file_path" > "$temp_file"
    
    # 在文件开头添加注释
    echo "// 测试函数使用从主包导入的函数" > "$file_path"
    cat "$temp_file" >> "$file_path"
  fi
  
  # 删除临时文件
  rm -f "$temp_file"
}

# 修复 azimuth 测试文件
echo "=== 修复 azimuth 测试文件 ==="
cd "$AZIMUTH_TEST_PATH"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    # 跳过 test_shared.mbt 和 test_helper.mbt
    if [ "$file" != "test_shared.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
      fix_file "$file"
    fi
  fi
done

# 修复 clean_test 测试文件
echo ""
echo "=== 修复 clean_test 测试文件 ==="
cd "$CLEAN_TEST_PATH"
for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    fix_file "$file"
  fi
done

echo ""
echo "修复完成！"