#!/bin/bash

# 批量修复测试文件中的字符串断言问题
# 将assert_eq("string", ...)替换为assert_eq_string("string", ...)

echo "开始修复测试文件中的字符串断言问题..."

# 查找所有需要修复的文件
files=$(find src -name "*.mbt" -type f -exec grep -l 'assert_eq(".*",.*@.*greet' {} \;)

for file in $files; do
    echo "修复文件: $file"
    
    # 将assert_eq("string", ...)替换为assert_eq_string("string", ...)
    sed -i 's/assert_eq("/assert_eq_string("/g' "$file"
done

echo "修复完成！"