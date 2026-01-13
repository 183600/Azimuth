#!/bin/bash

# 最终修复 clean_test 包中的函数调用语法

# 测试文件目录
TEST_DIR="src/clean_test/test"

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
    # 跳过非测试文件
    if [[ ! -f "$test_file" ]]; then
        continue
    fi
    
    echo "正在修复文件: $test_file"
    
    # 修复函数调用语法
    perl -i -pe 's/clean_test\.add\(/@clean_test.add(/g;' "$test_file"
    perl -i -pe 's/clean_test\.multiply\(/@clean_test.multiply(/g;' "$test_file"
    perl -i -pe 's/clean_test\.greet\(/@clean_test.greet(/g;' "$test_file"
    perl -i -pe 's/clean_test\.assert_eq\(/@clean_test.assert_eq(/g;' "$test_file"
    perl -i -pe 's/clean_test\.assert_eq_string\(/@clean_test.assert_eq_string(/g;' "$test_file"
    perl -i -pe 's/clean_test\.assert_true\(/@clean_test.assert_true(/g;' "$test_file"
    perl -i -pe 's/clean_test\.assert_false\(/@clean_test.assert_false(/g;' "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有 clean_test 测试文件语法修复完成！"