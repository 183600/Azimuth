#!/bin/bash

# 修复 clean_test 测试文件中的字符串测试

echo "修复 clean_test 测试文件中的字符串测试..."

# 找到所有 clean_test 的测试文件
find /home/runner/work/Azimuth/Azimuth/src/clean_test/test -name "*.mbt" -type f | while read file; do
    echo "修复 $file 中的字符串测试..."
    # 将 assert_eq("Hello, ..., @clean_test.greet(...)) 替换为 assert_eq_string("Hello, ..., @clean_test.greet(...))
    sed -i 's/assert_eq("Hello, \([^"]*\)", @clean_test\.greet(\([^)]*\)))/assert_eq_string("Hello, \1", @clean_test.greet(\2))/g' "$file"
    # 修复其他可能的字符串测试
    sed -i 's/assert_eq("\([^"]*\)", @clean_test\.greet(\([^)]*\)))/assert_eq_string("\1", @clean_test.greet(\2))/g' "$file"
done

echo "修复完成！"