#!/bin/bash

# 批量修复测试文件中的函数调用，添加@azimuth前缀

cd src/azimuth/test

# 需要修复的文件列表
FILES=(
    "new_tests.mbt"
    "simple_test_verify.mbt"
    "new_standard_tests.mbt"
    "additional_tests.mbt"
    "simple_test.mbt"
    "simple_new_tests.mbt"
    "concise_functional_tests.mbt"
    "additional_unique_tests.mbt"
    "comprehensive_edge_tests.mbt"
    "comprehensive_tests.mbt"
)

# 修复每个文件
for file in "${FILES[@]}"; do
    if [ -f "$file" ]; then
        echo "Fixing $file..."
        # 替换add(为@azimuth.add(
        sed -i 's/add(/@azimuth.add(/g' "$file"
        # 替换multiply(为@azimuth.multiply(
        sed -i 's/multiply(/@azimuth.multiply(/g' "$file"
        # 替换greet(为@azimuth.greet(
        sed -i 's/greet(/@azimuth.greet(/g' "$file"
        # 替换divide(为@azimuth.divide(
        sed -i 's/divide(/@azimuth.divide(/g' "$file"
    fi
done

echo "Done!"