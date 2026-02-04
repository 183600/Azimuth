#!/bin/bash

# 修复未绑定标识符的脚本

# 遍历所有测试文件
for file in $(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt"); do
    # 检查文件是否包含未绑定的标识符
    if grep -q "assert_eq\|add\|multiply\|greet\|divide_with_ceil\|subtract\|assert_eq_string\|assert_true\|assert_false" "$file"; then
        # 检查文件是否已经使用了@azimuth前缀
        if ! grep -q "@azimuth\." "$file"; then
            echo "Fixing $file"
            # 使用sed替换函数调用
            sed -i 's/\bassert_eq(/@azimuth.assert_eq(/g' "$file"
            sed -i 's/\bassert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
            sed -i 's/\bassert_true(/@azimuth.assert_true(/g' "$file"
            sed -i 's/\bassert_false(/@azimuth.assert_false(/g' "$file"
            sed -i 's/\badd(/@azimuth.add(/g' "$file"
            sed -i 's/\bmultiply(/@azimuth.multiply(/g' "$file"
            sed -i 's/\bgreet(/@azimuth.greet(/g' "$file"
            sed -i 's/\bdivide_with_ceil(/@azimuth.divide_with_ceil(/g' "$file"
            sed -i 's/\bsubtract(/@azimuth.subtract(/g' "$file"
        fi
    fi
done

echo "Done fixing unbound identifiers"