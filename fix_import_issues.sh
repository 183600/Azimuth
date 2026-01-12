#!/bin/bash

# 修复测试文件中的导入问题
# 将 azimuth:: 改为 @azimuth.

echo "Fixing import issues in test files..."

# 修复 azimuth/test 目录中的文件
find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" -type f | while read file; do
    # 跳过backup目录
    if [[ "$file" == *"/backup/"* ]]; then
        continue
    fi
    
    # 检查文件是否包含 azimuth::
    if grep -q "azimuth::" "$file"; then
        echo "Fixing $file"
        # 将 azimuth:: 改为 @azimuth.
        sed -i 's/azimuth::/@azimuth./g' "$file"
    fi
done

# 修复 clean_test/test 目录中的文件
find /home/runner/work/Azimuth/Azimuth/src/clean_test/test -name "*.mbt" -type f | while read file; do
    # 跳过backup目录
    if [[ "$file" == *"/backup/"* ]]; then
        continue
    fi
    
    # 检查文件是否包含 clean_test::
    if grep -q "clean_test::" "$file"; then
        echo "Fixing $file"
        # 将 clean_test:: 改为 @clean_test.
        sed -i 's/clean_test::/@clean_test./g' "$file"
    fi
    
    # 检查文件是否包含 azimuth::
    if grep -q "azimuth::" "$file"; then
        echo "Fixing $file"
        # 将 azimuth:: 改为 @azimuth.
        sed -i 's/azimuth::/@azimuth./g' "$file"
    fi
done

echo "Import issues fixed!"