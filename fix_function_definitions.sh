#!/bin/bash

# 修复azimuth测试文件中的函数定义问题

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "修复文件: $file"
    
    # 修复函数定义
    sed -i 's/pub fn @azimuth\./pub fn /g' "$file"
    
    # 修复函数调用 - 只修复调用，不修复定义
    # 首先保存原始文件
    cp "$file" "${file}.bak"
    
    # 使用更精确的方法修复函数调用
    python3 << EOF
with open('$file', 'r') as f:
    content = f.read()

# 修复函数调用，但不修复函数定义
import re

# 匹配函数定义并跳过
lines = content.split('\n')
in_function_def = False
for i, line in enumerate(lines):
    if line.strip().startswith('pub fn assert_eq'):
        in_function_def = True
    elif in_function_def and line.strip().startswith('}'):
        in_function_def = False
    elif not in_function_def and not line.strip().startswith('//') and not line.strip().startswith('pub fn'):
        # 修复函数调用
        line = re.sub(r'\bassert_eq\(', '@azimuth.assert_eq(', line)
        line = re.sub(r'\bassert_eq_string\(', '@azimuth.assert_eq_string(', line)
        line = re.sub(r'\bassert_true\(', '@azimuth.assert_true(', line)
        line = re.sub(r'\bassert_false\(', '@azimuth.assert_false(', line)
        line = re.sub(r'\bgreet\(', '@azimuth.greet(', line)
        line = re.sub(r'\badd\(', '@azimuth.add(', line)
        line = re.sub(r'\bmultiply\(', '@azimuth.multiply(', line)
        
        # 修复字符串比较
        line = re.sub(r'@azimuth\.assert_eq\(([^,]+),\s*@azimuth\.greet\(', '@azimuth.assert_eq_string(\1, @azimuth.greet(', line)
        
        # 修复长度比较
        line = re.sub(r'@azimuth\.assert_eq_string\(([^,]+),\s*[^.]+\.length\(\)', '@azimuth.assert_eq(\1, \2.length()', line)
    
    lines[i] = line

with open('$file', 'w') as f:
    f.write('\n'.join(lines))
EOF
    
    echo "已修复 $file"
  fi
done

echo "所有测试文件的函数定义问题已修复完成"