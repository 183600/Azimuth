#!/bin/bash

# 批量修复测试文件，使其使用正确的包函数

echo "修复azimuth包的测试文件..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

AZIMUTH_FILES=$(find . -name "*.mbt" -type f | grep -v backup)

for file in $AZIMUTH_FILES; do
    echo -n "修复 $file ... "
    
    # 检查文件是否包含需要替换的内容
    if grep -q "2 + 3" "$file" || grep -q "2 \* 3" "$file" || grep -q "\"Hello, \" + \"World\" + \"!\"" "$file"; then
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 替换操作
        sed -e 's/2 + 3/azimuth::add(2, 3)/g' \
            -e 's/5 + -5/azimuth::add(5, -5)/g' \
            -e 's/-7 + -3/azimuth::add(-7, -3)/g' \
            -e 's/2 \* 3/azimuth::multiply(2, 3)/g' \
            -e 's/2 \* -3/azimuth::multiply(2, -3)/g' \
            -e 's/-7 \* -3/azimuth::multiply(-7, -3)/g' \
            -e 's/"Hello, " + "World" + "!"/azimuth::greet("World")/g' \
            -e 's/"Hello, " + "" + "!"/azimuth::greet("")/g' \
            -e 's/"Hello, " + "Azimuth" + "!"/azimuth::greet("Azimuth")/g' \
            "$file" > "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$file"
        echo "✓ 完成"
    else
        echo "- 跳过（无需修改）"
    fi
done

echo ""
echo "修复clean_test包的测试文件..."
cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

CLEAN_TEST_FILES=$(find . -name "*.mbt" -type f | grep -v backup)

for file in $CLEAN_TEST_FILES; do
    echo -n "修复 $file ... "
    
    # 检查文件是否包含需要替换的内容
    if grep -q "2 + 3" "$file" || grep -q "2 \* 3" "$file" || grep -q "\"Hello, \" + \"World\" + \"!\"" "$file"; then
        # 创建临时文件
        temp_file=$(mktemp)
        
        # 替换操作
        sed -e 's/2 + 3/clean_test::add(2, 3)/g' \
            -e 's/5 + -5/clean_test::add(5, -5)/g' \
            -e 's/-7 + -3/clean_test::add(-7, -3)/g' \
            -e 's/2 \* 3/clean_test::multiply(2, 3)/g' \
            -e 's/2 \* -3/clean_test::multiply(2, -3)/g' \
            -e 's/-7 \* -3/clean_test::multiply(-7, -3)/g' \
            -e 's/"Hello, " + "World" + "!"/clean_test::greet("World")/g' \
            -e 's/"Hello, " + "" + "!"/clean_test::greet("")/g' \
            -e 's/"Hello, " + "Azimuth" + "!"/clean_test::greet("Azimuth")/g' \
            "$file" > "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$file"
        echo "✓ 完成"
    else
        echo "- 跳过（无需修改）"
    fi
done

echo ""
echo "所有测试文件修复完成！"