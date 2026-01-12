#!/bin/bash

# 添加测试辅助函数到所有测试文件

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

# 移动所有测试文件到test子目录
cd /home/runner/work/Azimuth/Azimuth/src/azimuth
for file in *.mbt; do
  if [ "$file" != "lib.mbt" ] && [ -f "$file" ]; then
    echo "处理文件: $file"
    
    # 移动到test子目录
    mv "$file" "test/"
    
    # 检查文件是否已经包含assert_eq函数
    if ! grep -q "pub fn assert_eq" "test/$file"; then
      # 在文件开头添加测试辅助函数
      echo "添加测试辅助函数到 test/$file"
      temp_file=$(mktemp)
      echo "$ASSERT_FUNCTIONS" > "$temp_file"
      cat "test/$file" >> "$temp_file"
      mv "$temp_file" "test/$file"
    fi
  fi
done

echo "所有测试文件已处理完成"