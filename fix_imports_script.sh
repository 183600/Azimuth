#!/bin/bash

# 修复 azimuth 测试文件中的导入问题
echo "修复 azimuth 测试文件中的导入问题..."

# 遍历所有测试文件
for file in src/azimuth/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        # 删除 import "../azimuth" 行
        sed -i '/^import "..\/azimuth"$/d' "$file"
        # 删除相关的注释行
        sed -i '/^\/\/ 测试函数和断言函数从 azimuth 包导入$/d' "$file"
    fi
done

echo "azimuth 测试文件导入问题修复完成"

# 修复 clean_test 测试文件中的导入问题
echo "修复 clean_test 测试文件中的导入问题..."

# 遍历所有测试文件
for file in src/clean_test/test/*.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        # 删除 import "../clean_test" 行
        sed -i '/^import "..\/clean_test"$/d' "$file"
        # 删除相关的注释行
        sed -i '/^\/\/ 测试函数和断言函数从 clean_test 包导入$/d' "$file"
    fi
done

echo "clean_test 测试文件导入问题修复完成"

echo "所有测试文件导入问题修复完成"