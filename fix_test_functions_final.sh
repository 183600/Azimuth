#!/bin/bash

# 最终修复版本：为测试文件添加正确的包前缀

# 测试文件目录
TEST_DIR="src/azimuth/test"

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
    # 跳过非测试文件
    if [[ ! -f "$test_file" ]]; then
        continue
    fi
    
    echo "正在修复文件: $test_file"
    
    # 使用 Perl 进行更可靠的替换
    perl -i -pe '
        s/\badd\(/azimuth.add(/g;
        s/\bmultiply\(/azimuth.multiply(/g;
        s/\bgreet\(/azimuth.greet(/g;
        s/\bassert_eq\(/azimuth.assert_eq(/g;
        s/\bassert_eq_string\(/azimuth.assert_eq_string(/g;
        s/\bassert_true\(/azimuth.assert_true(/g;
        s/\bassert_false\(/azimuth.assert_false(/g;
    ' "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有测试文件前缀修复完成！"