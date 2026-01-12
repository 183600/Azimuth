#!/bin/bash

# 修复 azimuth 测试文件中的双重包名前缀问题
# 将 azimuth.azimuth.function 改为 azimuth.function

echo "Fixing double package prefixes in azimuth test files..."

# 查找所有需要修复的文件
find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt" -type f | while read file; do
    # 检查文件是否包含 azimuth.azimuth. 模式
    if grep -q "azimuth\.azimuth\." "$file"; then
        echo "Fixing: $file"
        # 使用 sed 进行替换
        sed -i 's/azimuth\.azimuth\./azimuth\./g' "$file"
    fi
done

echo "Done fixing double package prefixes."