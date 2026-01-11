#!/bin/bash

# 修复 clean_test 包中的函数调用前缀

echo "修复 clean_test 包的测试文件..."

# 将 @azimuth 替换为 @clean_test
find /home/runner/work/Azimuth/Azimuth/src/clean_test/test -name "*.mbt" -type f | while read file; do
    echo "处理 $file..."
    sed -i 's/@azimuth\./@clean_test\./g' "$file"
done

echo "修复完成！"