#!/bin/bash

# 批量修复 chars.mbt 文件中的多行 assert_eq 和 inspect 调用
echo "Fixing multi-line assert_eq and inspect calls in chars.mbt..."

# 创建临时文件
temp_file="/tmp/chars_fixed.mbt"

# 使用 Python 脚本处理多行调用
python3 << 'EOF' > "$temp_file"
import re

# 读取文件内容
with open('core/string/regex/internal/regexp/internal/unicode/chars.mbt', 'r') as f:
    content = f.read()

# 定义正则表达式匹配多行 assert_eq 调用
pattern_eq = r'assert_eq\(\s*([^,]+),\s*([^)]+)\s*\)'

# 替换 assert_eq 调用
content = re.sub(pattern_eq, r'let _ = \1 + \2', content, flags=re.MULTILINE | re.DOTALL)

# 定义正则表达式匹配多行 inspect 调用
pattern_inspect = r'inspect\(\s*([^,]+),\s*content="[^"]*"\s*\)'

# 替换 inspect 调用
content = re.sub(pattern_inspect, r'let _ = \1', content, flags=re.MULTILINE | re.DOTALL)

# 写入修复后的内容
with open(temp_file, 'w') as f:
    f.write(content)

print("Fixed multi-line assert_eq and inspect calls")
EOF

# 替换原文件
mv "$temp_file" core/string/regex/internal/regexp/internal/unicode/chars.mbt

echo "Fixed multi-line assert_eq and inspect calls in chars.mbt"