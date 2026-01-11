#!/bin/bash

# 修复 assert_eq 函数调用的脚本 - 最终版

# 查找所有包含 assert_eq 的 .mbt 文件
find /home/runner/work/Azimuth/Azimuth/src -name "*.mbt" -exec grep -l "assert_eq" {} \; | while read file; do
    echo "修复文件: $file"
    
    # 使用 Python 进行更精确的文本替换
    python3 -c "
import re
import sys

# 读取文件内容
with open('$file', 'r') as f:
    content = f.read()

# 定义替换函数
def replace_assert_eq(match):
    args = match.group(1)
    # 分割参数，考虑嵌套的函数调用
    level = 0
    split_pos = -1
    for i, char in enumerate(args):
        if char == '(':
            level += 1
        elif char == ')':
            level -= 1
        elif char == ',' and level == 0:
            split_pos = i
            break
    
    if split_pos > 0:
        expected = args[:split_pos].strip()
        actual = args[split_pos+1:].strip()
        return f'if {expected} != {actual} {{ @test.fail(\"Test failed\") }}'
    else:
        return match.group(0)

# 替换 assert_eq 调用
pattern = r'assert_eq\(([^)]+)\)'
content = re.sub(pattern, replace_assert_eq, content)

# 写回文件
with open('$file', 'w') as f:
    f.write(content)
"
done

echo "assert_eq 修复完成"