#!/bin/bash

# 批量修复测试文件，移除包前缀

echo "修复azimuth包的测试文件，移除包前缀..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

AZIMUTH_FILES=$(find . -name "*.mbt" -type f | grep -v backup)

for file in $AZIMUTH_FILES; do
    echo -n "修复 $file ... "
    
    # 检查文件是否包含需要替换的内容
    if grep -q "azimuth/azimuth::" "$file"; then
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 替换操作
        sed -e 's/azimuth\/azimuth:://g' "$file" > "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$file"
        echo "✓ 完成"
    else
        echo "- 跳过（无需修改）"
    fi
done

echo ""
echo "修复clean_test包的测试文件，移除包前缀..."
cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

CLEAN_TEST_FILES=$(find . -name "*.mbt" -type f | grep -v backup)

for file in $CLEAN_TEST_FILES; do
    echo -n "修复 $file ... "
    
    # 检查文件是否包含需要替换的内容
    if grep -q "clean_test/clean_test::" "$file"; then
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 替换操作
        sed -e 's/clean_test\/clean_test:://g' "$file" > "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$file"
        echo "✓ 完成"
    else
        echo "- 跳过（无需修改）"
    fi
done

echo ""
echo "所有测试文件前缀修复完成！"