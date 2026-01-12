#!/bin/bash

# 修复测试文件中的重复函数定义问题

echo "Fixing duplicate function definitions in test files..."

# 设置路径
TEST_PATH="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"

# 进入测试目录
cd "$TEST_PATH"

# 获取所有测试文件
TEST_FILES=$(ls *.mbt 2>/dev/null | grep -v "moon.pkg.json" | grep -v "test_helper.mbt")

echo "Found test files: $TEST_FILES"

# 修复每个测试文件
for test_file in $TEST_FILES; do
    echo "Fixing $test_file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 检查文件是否包含函数定义
    if grep -q "fn add(" "$test_file"; then
        # 移除函数定义部分，保留测试部分
        awk '
        BEGIN { in_functions = 0; skip_until_tests = 0 }
        
        # 检测函数定义开始
        /fn add\(/ { 
            in_functions = 1
            skip_until_tests = 1
            next
        }
        
        # 如果在跳过模式，且遇到测试定义，则停止跳过
        skip_until_tests && /^test/ {
            skip_until_tests = 0
            in_functions = 0
        }
        
        # 如果不在跳过模式，打印行
        !skip_until_tests {
            print
        }
        ' "$test_file" > "$temp_file"
        
        # 替换原文件
        mv "$temp_file" "$test_file"
        echo "  Removed duplicate function definitions from $test_file"
    else
        echo "  No duplicate functions found in $test_file"
    fi
done

echo "Fixing duplicate test names..."

# 修复重复的测试名称问题
counter=1
for test_file in $TEST_FILES; do
    echo "Fixing test names in $test_file..."
    
    # 创建临时文件
    temp_file=$(mktemp)
    
    # 为测试添加唯一前缀
    awk -v prefix="test_${counter}_" '
    /^test/ {
        # 提取测试名称
        if (match($0, /test "([^"]+)"/, arr)) {
            original_name = arr[1]
            # 替换为带前缀的名称
            replacement = "test \"" prefix original_name "\""
            sub(/test "[^"]+"/, replacement)
        }
    }
    { print }
    ' "$test_file" > "$temp_file"
    
    # 替换原文件
    mv "$temp_file" "$test_file"
    
    counter=$((counter + 1))
done

echo "Done fixing test files."