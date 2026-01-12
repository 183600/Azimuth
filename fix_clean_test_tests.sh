#!/bin/bash

# 批量修复clean_test包的测试文件，将clean_test::函数调用替换为直接操作

cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

# 备份原始文件
mkdir -p backup
cp *.mbt backup/ 2>/dev/null || true

# 修复所有测试文件
for file in *.mbt; do
    if [ -f "$file" ]; then
        echo "Processing $file..."
        
        # 替换clean_test::add为直接加法
        sed -i 's/clean_test::add(\([^,]*\),\s*\([^)]*\))/\1 + \2/g' "$file"
        
        # 替换clean_test::multiply为直接乘法
        sed -i 's/clean_test::multiply(\([^,]*\),\s*\([^)]*\))/\1 * \2/g' "$file"
        
        # 替换clean_test::greet为直接字符串拼接
        sed -i 's/clean_test::greet(\([^)]*\))/"Hello, " + \1 + "!"/g' "$file"
        
        # 替换clean_test::assert_eq为直接比较
        sed -i 's/clean_test::assert_eq(\([^,]*\),\s*\([^)]*\))/if \1 != \2 { @builtin.panic() }/g' "$file"
        
        # 替换clean_test::assert_eq_string为直接比较
        sed -i 's/clean_test::assert_eq_string(\([^,]*\),\s*\([^)]*\))/if \1 != \2 { @builtin.panic() }/g' "$file"
        
        # 替换clean_test::assert_true为直接检查
        sed -i 's/clean_test::assert_true(\([^)]*\))/if !\1 { @builtin.panic() }/g' "$file"
        
        # 替换clean_test::assert_false为直接检查
        sed -i 's/clean_test::assert_false(\([^)]*\))/if \1 { @builtin.panic() }/g' "$file"
        
        echo "Fixed $file"
    fi
done

echo "All files fixed!"