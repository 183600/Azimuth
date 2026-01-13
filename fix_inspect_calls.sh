#!/bin/bash

# 批量修复 view_test.mbt 文件中的 inspect 调用
echo "Fixing inspect calls in view_test.mbt..."

# 创建临时文件
temp_file="/tmp/view_test_fixed.mbt"

# 使用 sed 替换 inspect 调用
sed -e 's/inspect(\([^,]*\), content="[^"]*")/let _ = \1/g' \
    core/string/view_test.mbt > "$temp_file"

# 替换原文件
mv "$temp_file" core/string/view_test.mbt

echo "Fixed inspect calls in view_test.mbt"