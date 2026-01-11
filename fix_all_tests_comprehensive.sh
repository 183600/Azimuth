#!/bin/bash

# 完整修复所有测试文件

echo "Comprehensive fix for all test files..."

# 首先备份原始文件
echo "Backing up original files..."
mkdir -p backup_test_files
cp src/azimuth/test/*.mbt backup_test_files/ 2>/dev/null || true
cp src/clean_test/test/*.mbt backup_test_files/ 2>/dev/null || true

# 修复 azimuth/test 目录下的文件
cd src/azimuth/test

# 创建一个Python脚本来正确处理修复
cat > fix_all_tests.py << 'EOF'
import re
import os

def fix_test_file(content):
    # 修复 assert_eq 调用
    # 匹配 assert_eq(actual, expected) 或 assert_eq(expected, actual)
    lines = content.split('\n')
    fixed_lines = []
    
    for line in lines:
        # 跳过注释行
        if line.strip().startswith('//'):
            fixed_lines.append(line)
            continue
            
        # 查找 assert_eq 调用
        if 'assert_eq(' in line:
            # 使用正则表达式匹配 assert_eq 调用
            pattern = r'assert_eq\s*\(\s*([^,]+?),\s*([^)]+?)\s*\)'
            matches = re.findall(pattern, line)
            
            if matches:
                for match in matches:
                    actual = match[0].strip()
                    expected = match[1].strip()
                    # 替换为 if 语句
                    replacement = f'if {expected} != {actual} {{ @test.fail("Test failed") }}'
                    line = re.sub(pattern, replacement, line)
        
        fixed_lines.append(line)
    
    return '\n'.join(fixed_lines)

# 处理每个文件
for filename in os.listdir('.'):
    if filename.endswith('.mbt') and filename not in ['simple_test.mbt', 'basic_test.mbt']:
        print(f"Processing {filename}...")
        
        try:
            with open(filename, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # 修复内容
            fixed_content = fix_test_file(content)
            
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(fixed_content)
        except Exception as e:
            print(f"Error processing {filename}: {e}")

print("Fixed all test files")
EOF

# 运行Python脚本
python3 fix_all_tests.py

# 删除临时脚本
rm fix_all_tests.py

echo "Fixed azimuth test files"

# 修复 clean_test/test 目录下的文件
cd ../../clean_test/test

# 再次创建Python脚本
cat > fix_all_tests.py << 'EOF'
import re
import os

def fix_test_file(content):
    # 修复 assert_eq 调用
    lines = content.split('\n')
    fixed_lines = []
    
    for line in lines:
        # 跳过注释行
        if line.strip().startswith('//'):
            fixed_lines.append(line)
            continue
            
        # 查找 assert_eq 调用
        if 'assert_eq(' in line:
            # 使用正则表达式匹配 assert_eq 调用
            pattern = r'assert_eq\s*\(\s*([^,]+?),\s*([^)]+?)\s*\)'
            matches = re.findall(pattern, line)
            
            if matches:
                for match in matches:
                    actual = match[0].strip()
                    expected = match[1].strip()
                    # 替换为 if 语句
                    replacement = f'if {expected} != {actual} {{ @test.fail("Test failed") }}'
                    line = re.sub(pattern, replacement, line)
        
        fixed_lines.append(line)
    
    return '\n'.join(fixed_lines)

# 处理每个文件
for filename in os.listdir('.'):
    if filename.endswith('.mbt') and filename not in ['simple_test.mbt']:
        print(f"Processing {filename}...")
        
        try:
            with open(filename, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # 修复内容
            fixed_content = fix_test_file(content)
            
            with open(filename, 'w', encoding='utf-8') as f:
                f.write(fixed_content)
        except Exception as e:
            print(f"Error processing {filename}: {e}")

print("Fixed all test files")
EOF

# 运行Python脚本
python3 fix_all_tests.py

# 删除临时脚本
rm fix_all_tests.py

echo "Fixed clean_test test files"

echo "Done!"
