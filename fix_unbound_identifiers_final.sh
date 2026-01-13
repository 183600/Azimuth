#!/bin/bash

# 修复所有测试文件中的未绑定标识符问题
# 将直接使用的函数调用添加适当的前缀

echo "开始修复测试文件中的未绑定标识符..."

# 修复azimuth包中的测试文件
AZIMUTH_TEST_DIR="src/azimuth/test"
if [ -d "$AZIMUTH_TEST_DIR" ]; then
    echo "修复azimuth包中的测试文件..."
    
    # 查找所有.mbt文件并修复未绑定的标识符
    find "$AZIMUTH_TEST_DIR" -name "*.mbt" -type f | while read file; do
        echo "处理文件: $file"
        
        # 备份原文件
        cp "$file" "$file.backup"
        
        # 修复函数调用，添加@azimuth.前缀
        sed -i 's/\bassert_eq(/@azimuth.assert_eq(/g' "$file"
        sed -i 's/\bassert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
        sed -i 's/\bassert_true(/@azimuth.assert_true(/g' "$file"
        sed -i 's/\bassert_false(/@azimuth.assert_false(/g' "$file"
        sed -i 's/\badd(/@azimuth.add(/g' "$file"
        sed -i 's/\bmultiply(/@azimuth.multiply(/g' "$file"
        sed -i 's/\bgreet(/@azimuth.greet(/g' "$file"
        
        # 检查是否有test_only目录中的文件需要特殊处理
        if [[ "$file" == *"test_only"* ]]; then
            echo "跳过test_only目录中的文件: $file"
            # 恢复备份
            mv "$file.backup" "$file"
        else
            # 删除备份文件
            rm "$file.backup"
        fi
    done
fi

# 修复clean_test包中的测试文件
CLEAN_TEST_DIR="src/clean_test/test"
if [ -d "$CLEAN_TEST_DIR" ]; then
    echo "修复clean_test包中的测试文件..."
    
    # 查找所有.mbt文件并修复未绑定的标识符
    find "$CLEAN_TEST_DIR" -name "*.mbt" -type f | while read file; do
        echo "处理文件: $file"
        
        # 备份原文件
        cp "$file" "$file.backup"
        
        # 修复函数调用，添加@clean_test.前缀
        sed -i 's/\bassert_eq(/@clean_test.assert_eq(/g' "$file"
        sed -i 's/\bassert_eq_string(/@clean_test.assert_eq_string(/g' "$file"
        sed -i 's/\bassert_true(/@clean_test.assert_true(/g' "$file"
        sed -i 's/\bassert_false(/@clean_test.assert_false(/g' "$file"
        sed -i 's/\badd(/@clean_test.add(/g' "$file"
        sed -i 's/\bmultiply(/@clean_test.multiply(/g' "$file"
        sed -i 's/\bgreet(/@clean_test.greet(/g' "$file"
        
        # 检查是否有test_only目录中的文件需要特殊处理
        if [[ "$file" == *"test_only"* ]]; then
            echo "跳过test_only目录中的文件: $file"
            # 恢复备份
            mv "$file.backup" "$file"
        else
            # 删除备份文件
            rm "$file.backup"
        fi
    done
fi

echo "修复完成！"