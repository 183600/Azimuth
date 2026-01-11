#!/bin/bash

# 修复测试文件中的assert_eq问题 - 改进版

echo "Fixing assert_eq in test files (improved version)..."

# 修复 azimuth/test 目录下的文件
cd src/azimuth/test

# 创建一个临时Python脚本来正确处理替换
cat > fix_assert_eq.py << 'EOF'
import re
import os

def fix_assert_eq(content):
    # 匹配 assert_eq(actual, expected) 格式
    pattern = r'assert_eq\s*\(\s*([^,]+?),\s*([^)]+?)\s*\)'
    
    def replacement(match):
        actual = match.group(1).strip()
        expected = match.group(2).strip()
        return f'if {expected} != {actual} {{ @test.fail("Test failed") }}'
    
    return re.sub(pattern, replacement, content)

# 处理每个文件
for filename in os.listdir('.'):
    if filename.endswith('.mbt') and filename not in ['simple_test.mbt', 'basic_test.mbt']:
        print(f"Processing {filename}...")
        
        with open(filename, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # 修复内容
        fixed_content = fix_assert_eq(content)
        
        with open(filename, 'w', encoding='utf-8') as f:
            f.write(fixed_content)

print("Fixed files")
EOF

# 运行Python脚本
python3 fix_assert_eq.py

# 删除临时脚本
rm fix_assert_eq.py

echo "Fixed azimuth test files"

# 修复 clean_test/test 目录下的文件
cd ../../clean_test/test

# 再次创建Python脚本
cat > fix_assert_eq.py << 'EOF'
import re
import os

def fix_assert_eq(content):
    # 匹配 assert_eq(actual, expected) 格式
    pattern = r'assert_eq\s*\(\s*([^,]+?),\s*([^)]+?)\s*\)'
    
    def replacement(match):
        actual = match.group(1).strip()
        expected = match.group(2).strip()
        return f'if {expected} != {actual} {{ @test.fail("Test failed") }}'
    
    return re.sub(pattern, replacement, content)

# 处理每个文件
for filename in os.listdir('.'):
    if filename.endswith('.mbt') and filename not in ['simple_test.mbt']:
        print(f"Processing {filename}...")
        
        with open(filename, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # 修复内容
        fixed_content = fix_assert_eq(content)
        
        with open(filename, 'w', encoding='utf-8') as f:
            f.write(fixed_content)

print("Fixed files")
EOF

# 运行Python脚本
python3 fix_assert_eq.py

# 删除临时脚本
rm fix_assert_eq.py

echo "Fixed clean_test test files"

echo "Done!"