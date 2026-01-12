#!/bin/bash

# 批量修复测试文件中的导入问题
# 删除所有测试文件中的 import "../azimuth" 和 import "../clean_test" 语句

echo "开始修复测试文件导入问题..."

# 修复 azimuth 测试文件
for file in src/azimuth/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        # 删除 import "../azimuth" 行
        sed -i '/^import "..\/azimuth"$/d' "$file"
    fi
done

# 修复 clean_test 测试文件  
for file in src/clean_test/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        # 删除 import "../clean_test" 行
        sed -i '/^import "..\/clean_test"$/d' "$file"
    fi
done

echo "修复完成！"