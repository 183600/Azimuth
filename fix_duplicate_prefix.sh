#!/bin/bash

# 修复重复前缀问题

echo "修复重复前缀问题..."

# 修复azimuth包中的测试文件
AZIMUTH_TEST_DIR="src/azimuth/test"
if [ -d "$AZIMUTH_TEST_DIR" ]; then
    echo "修复azimuth包中的测试文件的重复前缀..."
    
    # 查找所有.mbt文件并修复重复前缀
    find "$AZIMUTH_TEST_DIR" -name "*.mbt" -type f | while read file; do
        echo "处理文件: $file"
        
        # 修复重复前缀
        sed -i 's/@azimuth\.@azimuth\./@azimuth./g' "$file"
    done
fi

# 修复clean_test包中的测试文件
CLEAN_TEST_DIR="src/clean_test/test"
if [ -d "$CLEAN_TEST_DIR" ]; then
    echo "修复clean_test包中的测试文件的重复前缀..."
    
    # 查找所有.mbt文件并修复重复前缀
    find "$CLEAN_TEST_DIR" -name "*.mbt" -type f | while read file; do
        echo "处理文件: $file"
        
        # 修复重复前缀
        sed -i 's/@clean_test\.@clean_test\./@clean_test./g' "$file"
    done
fi

echo "修复完成！"