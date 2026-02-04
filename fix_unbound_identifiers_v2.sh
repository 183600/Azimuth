#!/bin/bash

# 更全面地修复未绑定标识符的脚本

# 遍历所有测试文件
for file in $(find /home/runner/work/Azimuth/Azimuth/src/azimuth/test -name "*.mbt"); do
    # 检查文件是否包含未绑定的标识符
    if grep -q "assert_eq\|add\|multiply\|greet\|divide_with_ceil\|subtract\|assert_eq_string\|assert_true\|assert_false" "$file"; then
        echo "Fixing $file"
        # 使用sed替换函数调用，包括前面没有@azimuth.的情况
        sed -i 's/\([^@.]\)assert_eq(/\1@azimuth.assert_eq(/g' "$file"
        sed -i 's/^assert_eq(/@azimuth.assert_eq(/g' "$file"
        sed -i 's/\([^@.]\)assert_eq_string(/\1@azimuth.assert_eq_string(/g' "$file"
        sed -i 's/^assert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
        sed -i 's/\([^@.]\)assert_true(/\1@azimuth.assert_true(/g' "$file"
        sed -i 's/^assert_true(/@azimuth.assert_true(/g' "$file"
        sed -i 's/\([^@.]\)assert_false(/\1@azimuth.assert_false(/g' "$file"
        sed -i 's/^assert_false(/@azimuth.assert_false(/g' "$file"
        sed -i 's/\([^@.]\)add(/\1@azimuth.add(/g' "$file"
        sed -i 's/^add(/@azimuth.add(/g' "$file"
        sed -i 's/\([^@.]\)multiply(/\1@azimuth.multiply(/g' "$file"
        sed -i 's/^multiply(/@azimuth.multiply(/g' "$file"
        sed -i 's/\([^@.]\)greet(/\1@azimuth.greet(/g' "$file"
        sed -i 's/^greet(/@azimuth.greet(/g' "$file"
        sed -i 's/\([^@.]\)divide_with_ceil(/\1@azimuth.divide_with_ceil(/g' "$file"
        sed -i 's/^divide_with_ceil(/@azimuth.divide_with_ceil(/g' "$file"
        sed -i 's/\([^@.]\)subtract(/\1@azimuth.subtract(/g' "$file"
        sed -i 's/^subtract(/@azimuth.subtract(/g' "$file"
    fi
done

echo "Done fixing unbound identifiers"