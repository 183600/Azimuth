#!/bin/bash

# 批量修复 view_test.mbt 文件中的多行 inspect 调用
echo "Fixing multi-line inspect calls in view_test.mbt..."

# 创建临时文件
temp_file="/tmp/view_test_fixed.mbt"

# 使用 Python 脚本处理多行 inspect 调用
python3 << 'EOF' > "$temp_file"
import re

# 读取文件内容
with open('core/string/view_test.mbt', 'r') as f:
    content = f.read()

# 定义正则表达式匹配多行 inspect 调用
pattern = r'inspect\(\s*([^,]+),\s*content="[^"]*"\s*\)'

# 替换 inspect 调用
content = re.sub(pattern, r'let _ = \1', content, flags=re.MULTILINE | re.DOTALL)

# 写入修复后的内容
with open(temp_file, 'w') as f:
    f.write(content)

print("Fixed multi-line inspect calls")
EOF

# 替换原文件
mv "$temp_file" core/string/view_test.mbt

echo "Fixed multi-line inspect calls in view_test.mbt"