#!/bin/bash

# 修复所有测试文件中缺少的导入语句
# 这个脚本会为所有显示"No tests found"的测试文件添加必要的导入

echo "开始修复 clean_test 测试文件的导入语句..."

# 定义测试目录
TEST_DIR="src/clean_test/test"

# 进入测试目录
cd "$TEST_DIR" || exit 1

# 遍历所有.mbt文件
for file in *.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        
        # 检查文件是否已经包含导入语句
        if ! grep -q "import clean_test" "$file"; then
            echo "  添加导入语句到 $file"
            
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 添加导入语句到文件开头
            echo "import clean_test" > "$temp_file"
            echo "" >> "$temp_file"
            cat "$file" >> "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
        else
            echo "  $file 已包含导入语句"
        fi
    fi
done

echo "修复完成！"

# 返回原目录
cd - > /dev/null