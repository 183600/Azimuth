#!/bin/bash

# 批量修复azimuth测试文件中的导入问题

echo "开始修复azimuth测试文件中的导入问题..."

# 获取所有azimuth测试文件
test_files=$(find src/azimuth/test -name "*.mbt" -type f)

# 对每个测试文件进行处理
for file in $test_files; do
    echo "处理文件: $file"
    
    # 检查文件是否包含未绑定的函数调用
    if grep -q "assert_eq\|assert_eq_string\|assert_true\|assert_false\|add\|multiply\|greet" "$file"; then
        # 检查是否已经有导入语句
        if ! grep -q "import" "$file"; then
            # 添加导入语句到文件开头
            temp_file=$(mktemp)
            echo "import \"../azimuth\"" > "$temp_file"
            cat "$file" >> "$temp_file"
            mv "$temp_file" "$file"
            echo "  已添加导入语句"
        else
            echo "  文件已包含导入语句"
        fi
    else
        echo "  文件无需修复"
    fi
done

echo "azimuth测试文件导入问题修复完成！"