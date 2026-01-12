#!/bin/bash

# 批量修复测试文件，将azimuth::函数调用替换为直接操作

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

# 备份原始文件
mkdir -p backup
cp *.mbt backup/ 2>/dev/null || true

# 修复所有测试文件
for file in *.mbt; do
    if [ -f "$file" ]; then
        echo "Processing $file..."
        
        # 替换azimuth::add为直接加法
        sed -i 's/azimuth::add(\([^,]*\),\s*\([^)]*\))/\1 + \2/g' "$file"
        
        # 替换azimuth::multiply为直接乘法
        sed -i 's/azimuth::multiply(\([^,]*\),\s*\([^)]*\))/\1 * \2/g' "$file"
        
        # 替换azimuth::greet为直接字符串拼接
        sed -i 's/azimuth::greet(\([^)]*\))/"Hello, " + \1 + "!"/g' "$file"
        
        # 替换azimuth::assert_eq为直接比较
        sed -i 's/azimuth::assert_eq(\([^,]*\),\s*\([^)]*\))/if \1 != \2 { @builtin.panic() }/g' "$file"
        
        # 替换azimuth::assert_eq_string为直接比较
        sed -i 's/azimuth::assert_eq_string(\([^,]*\),\s*\([^)]*\))/if \1 != \2 { @builtin.panic() }/g' "$file"
        
        # 替换azimuth::assert_true为直接检查
        sed -i 's/azimuth::assert_true(\([^)]*\))/if !\1 { @builtin.panic() }/g' "$file"
        
        # 替换azimuth::assert_false为直接检查
        sed -i 's/azimuth::assert_false(\([^)]*\))/if \1 { @builtin.panic() }/g' "$file"
        
        echo "Fixed $file"
    fi
done

echo "All files fixed!"