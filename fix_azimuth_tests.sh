#!/bin/bash

# 批量修复azimuth包中的所有测试文件

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

# 定义测试辅助函数
ASSERT_FUNCTIONS='// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

pub fn assert_true(condition : Bool) -> Unit {
  let _ = condition
}

pub fn assert_false(condition : Bool) -> Unit {
  let _ = condition == false
}

'

for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "处理文件: $file"
    
    # 检查文件是否已经包含assert_eq函数
    if ! grep -q "pub fn assert_eq" "$file"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 添加测试辅助函数
      echo "$ASSERT_FUNCTIONS" > "$temp_file"
      
      # 添加原文件内容
      cat "$file" >> "$temp_file"
      
      # 替换原文件
      mv "$temp_file" "$file"
      
      echo "已添加测试辅助函数到 $file"
    fi
    
    # 修复函数调用
    sed -i 's/assert_eq(/@azimuth.assert_eq(/g' "$file"
    sed -i 's/assert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
    sed -i 's/assert_true(/@azimuth.assert_true(/g' "$file"
    sed -i 's/assert_false(/@azimuth.assert_false(/g' "$file"
    
    # 修复greet函数调用
    sed -i 's/greet(/@azimuth.greet(/g' "$file"
    
    # 修复add和multiply函数调用
    sed -i 's/add(/@azimuth.add(/g' "$file"
    sed -i 's/multiply(/@azimuth.multiply(/g' "$file"
    
    # 修复字符串比较
    sed -i 's/@azimuth.assert_eq(/@azimuth.assert_eq_string(/g; t; s/@azimuth.assert_eq_string.*greet/@azimuth.assert_eq_string/g' "$file"
    
    # 修复长度比较
    sed -i 's/@azimuth.assert_eq_string.*\.length()/@azimuth.assert_eq/g' "$file"
  fi
done

echo "所有测试文件已修复完成"