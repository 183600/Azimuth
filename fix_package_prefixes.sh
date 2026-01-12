#!/bin/bash

# 批量修复测试文件的包名前缀

echo "修复azimuth包的测试文件前缀..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

AZIMUTH_FILES=$(find . -name "*.mbt" -type f | grep -v backup)

for file in $AZIMUTH_FILES; do
    echo -n "修复 $file ... "
    
    # 检查文件是否包含需要替换的内容
    if grep -q "azimuth::" "$file"; then
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 替换操作
        sed -e 's/azimuth::/azimuth\/azimuth::/g' "$file" > "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$file"
        echo "✓ 完成"
    else
        echo "- 跳过（无需修改）"
    fi
done

echo ""
echo "修复clean_test包的测试文件前缀..."
cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

# 首先检查clean_test包的正确名称
if [ -f "../pkg.generated.mbti" ]; then
    PACKAGE_NAME=$(grep 'package "' ../pkg.generated.mbti | sed 's/package "//' | sed 's/"//')
    echo "发现clean_test包名: $PACKAGE_NAME"
    
    CLEAN_TEST_FILES=$(find . -name "*.mbt" -type f | grep -v backup)
    
    for file in $CLEAN_TEST_FILES; do
        echo -n "修复 $file ... "
        
        # 检查文件是否包含需要替换的内容
        if grep -q "clean_test::" "$file"; then
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 替换操作
            sed -e "s/clean_test::/$PACKAGE_NAME::/g" "$file" > "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
            echo "✓ 完成"
        else
            echo "- 跳过（无需修改）"
        fi
    done
else
    echo "未找到clean_test包的pkg.generated.mbti文件"
    echo "使用默认包名clean_test/clean_test"
    
    CLEAN_TEST_FILES=$(find . -name "*.mbt" -type f | grep -v backup)
    
    for file in $CLEAN_TEST_FILES; do
        echo -n "修复 $file ... "
        
        # 检查文件是否包含需要替换的内容
        if grep -q "clean_test::" "$file"; then
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 替换操作
            sed -e 's/clean_test::/clean_test\/clean_test::/g' "$file" > "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
            echo "✓ 完成"
        else
            echo "- 跳过（无需修改）"
        fi
    done
fi

echo ""
echo "所有测试文件前缀修复完成！"