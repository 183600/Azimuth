#!/bin/bash

# 修复测试文件中的未绑定标识符问题
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"

echo "开始修复测试文件中的未绑定标识符问题..."
echo "只修复实际的测试文件，不修改库文件..."

# 查找所有测试目录中的 .mbt 文件
echo "查找测试目录中的.mbt文件..."

# 修复 azimuth/test 目录中的文件
if [ -d "$PROJECT_ROOT/azimuth/test" ]; then
    echo "修复 azimuth/test 目录中的文件..."
    find "$PROJECT_ROOT/azimuth/test" -name "*.mbt" -type f -not -path "*/backup/*" | while read file; do
        echo "检查文件: $file"
        # 检查文件是否包含未绑定的函数调用
        if grep -q "assert_eq(" "$file" && ! grep -q "@azimuth.assert_eq" "$file"; then
            echo "修复文件: $file"
            # 创建备份
            cp "$file" "$file.bak_fix_test"
            # 添加 @azimuth. 前缀
            sed -i 's/assert_eq(/@azimuth.assert_eq(/g' "$file"
            sed -i 's/assert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
            sed -i 's/assert_true(/@azimuth.assert_true(/g' "$file"
            sed -i 's/assert_false(/@azimuth.assert_false(/g' "$file"
            # 修复函数调用
            sed -i 's/add(/@azimuth.add(/g' "$file"
            sed -i 's/multiply(/@azimuth.multiply(/g' "$file"
            sed -i 's/greet(/@azimuth.greet(/g' "$file"
        fi
    done
fi

# 修复 clean_test/test 目录中的文件
if [ -d "$PROJECT_ROOT/clean_test/test" ]; then
    echo "修复 clean_test/test 目录中的文件..."
    find "$PROJECT_ROOT/clean_test/test" -name "*.mbt" -type f -not -path "*/backup/*" | while read file; do
        echo "检查文件: $file"
        # 检查文件是否包含未绑定的函数调用
        if grep -q "assert_eq(" "$file" && ! grep -q "@clean_test.assert_eq" "$file"; then
            echo "修复文件: $file"
            # 创建备份
            cp "$file" "$file.bak_fix_test"
            # 添加 @clean_test. 前缀
            sed -i 's/assert_eq(/@clean_test.assert_eq(/g' "$file"
            sed -i 's/assert_eq_string(/@clean_test.assert_eq_string(/g' "$file"
            sed -i 's/assert_true(/@clean_test.assert_true(/g' "$file"
            sed -i 's/assert_false(/@clean_test.assert_false(/g' "$file"
            # 修复函数调用
            sed -i 's/add(/@clean_test.add(/g' "$file"
            sed -i 's/multiply(/@clean_test.multiply(/g' "$file"
            sed -i 's/greet(/@clean_test.greet(/g' "$file"
        fi
    done
fi

# 修复 src/azimuth/test 目录中的文件
if [ -d "$PROJECT_ROOT/src/azimuth/test" ]; then
    echo "修复 src/azimuth/test 目录中的文件..."
    find "$PROJECT_ROOT/src/azimuth/test" -name "*.mbt" -type f -not -path "*/backup/*" | while read file; do
        echo "检查文件: $file"
        # 检查文件是否包含未绑定的函数调用
        if grep -q "assert_eq(" "$file" && ! grep -q "@azimuth.assert_eq" "$file"; then
            echo "修复文件: $file"
            # 创建备份
            cp "$file" "$file.bak_fix_test"
            # 添加 @azimuth. 前缀
            sed -i 's/assert_eq(/@azimuth.assert_eq(/g' "$file"
            sed -i 's/assert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
            sed -i 's/assert_true(/@azimuth.assert_true(/g' "$file"
            sed -i 's/assert_false(/@azimuth.assert_false(/g' "$file"
            # 修复函数调用
            sed -i 's/add(/@azimuth.add(/g' "$file"
            sed -i 's/multiply(/@azimuth.multiply(/g' "$file"
            sed -i 's/greet(/@azimuth.greet(/g' "$file"
        fi
    done
fi

echo "修复完成！"
echo "运行测试验证修复结果..."

cd "$PROJECT_ROOT"
./moon test