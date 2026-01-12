#!/bin/bash

# 更全面的批量修复测试文件导入问题

AZIMUTH_TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/azimuth/test"
CLEAN_TEST_TEST_DIR="/home/runner/work/Azimuth/Azimuth/src/clean_test/test"

# 修复azimuth测试文件
echo "修复azimuth测试文件..."
for file in "$AZIMUTH_TEST_DIR"/*.mbt; do
    if [ -f "$file" ]; then
        # 检查是否包含未绑定的标识符
        if grep -q "assert_eq\|assert_true\|add\|multiply\|greet" "$file"; then
            # 检查是否已经有导入语句
            if ! grep -q "import.*test_helper\|import.*azimuth" "$file"; then
                # 创建临时文件
                temp_file=$(mktemp)
                # 添加导入语句
                echo "import test_helper" > "$temp_file"
                echo "" >> "$temp_file"
                # 添加原文件内容
                cat "$file" >> "$temp_file"
                # 替换原文件
                mv "$temp_file" "$file"
            fi
        fi
    fi
done

# 修复clean_test测试文件
echo "修复clean_test测试文件..."
for file in "$CLEAN_TEST_TEST_DIR"/*.mbt; do
    if [ -f "$file" ]; then
        # 检查是否包含未绑定的标识符
        if grep -q "assert_eq\|assert_true\|add\|multiply\|greet" "$file"; then
            # 检查是否已经有导入语句
            if ! grep -q "import.*test_helper\|import.*clean_test" "$file"; then
                # 创建临时文件
                temp_file=$(mktemp)
                # 添加导入语句
                echo "import test_helper" > "$temp_file"
                echo "" >> "$temp_file"
                # 添加原文件内容
                cat "$file" >> "$temp_file"
                # 替换原文件
                mv "$temp_file" "$file"
            fi
        fi
    fi
done

echo "修复完成！"