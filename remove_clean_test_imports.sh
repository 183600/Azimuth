#!/bin/bash

# 移除所有测试文件中的 import 语句
# 因为测试包已经通过 test-import 导入了 clean_test

echo "开始移除 clean_test 测试文件中的导入语句..."

# 定义测试目录
TEST_DIR="src/clean_test/test"

# 进入测试目录
cd "$TEST_DIR" || exit 1

# 遍历所有.mbt文件
for file in *.mbt; do
    if [ -f "$file" ]; then
        echo "处理文件: $file"
        
        # 跳过 test_helper.mbt 文件
        if [ "$file" = "test_helper.mbt" ]; then
            echo "  跳过 test_helper.mbt"
            continue
        fi
        
        # 检查文件是否包含导入语句
        if grep -q "import clean_test" "$file"; then
            echo "  移除导入语句从 $file"
            
            # 创建临时文件
            temp_file=$(mktemp)
            
            # 移除第一行的 import 语句
            tail -n +3 "$file" > "$temp_file"
            
            # 替换原文件
            mv "$temp_file" "$file"
        else
            echo "  $file 不包含导入语句"
        fi
    fi
done

echo "修复完成！"

# 返回原目录
cd - > /dev/null