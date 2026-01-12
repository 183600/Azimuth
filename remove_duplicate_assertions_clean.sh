#!/bin/bash

# 删除测试文件中重复的断言函数定义
# 保留 test_helper.mbt 中的正确版本

for file in src/clean_test/test/*.mbt; do
  if [ "$file" != "src/clean_test/test/test_helper.mbt" ]; then
    echo "Processing $file..."
    
    # 检查文件是否包含断言函数定义
    if grep -q "pub fn assert_eq" "$file"; then
      # 创建临时文件
      temp_file=$(mktemp)
      
      # 删除断言函数定义部分（从 "// 断言相等函数" 到 "// 基础测试用例" 或类似的测试开始部分）
      awk '
        BEGIN { skip = 0 }
        /\/\/ 断言相等函数|\/\* 断言相等函数|pub fn assert_eq/ {
          skip = 1
        }
        /\/\/ 基础测试用例|\/\* 基础测试用例|test "/ {
          skip = 0
          if (!/^test/) print ""
        }
        {
          if (!skip) print
        }
      ' "$file" > "$temp_file"
      
      # 替换原文件
      mv "$temp_file" "$file"
      echo "Fixed $file"
    fi
  fi
done

echo "Done!"