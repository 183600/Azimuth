#!/bin/bash

# 移除 clean_test 包中的函数调用前缀

# 测试文件目录
TEST_DIR="src/clean_test/test"

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
    # 跳过非测试文件
    if [[ ! -f "$test_file" ]]; then
        continue
    fi
    
    echo "正在修复文件: $test_file"
    
    # 移除函数调用前缀
    perl -i -pe 's/\.add\(/add(/g;' "$test_file"
    perl -i -pe 's/\.multiply\(/multiply(/g;' "$test_file"
    perl -i -pe 's/\.greet\(/greet(/g;' "$test_file"
    perl -i -pe 's/\.assert_eq\(/assert_eq(/g;' "$test_file"
    perl -i -pe 's/\.assert_eq_string\(/assert_eq_string(/g;' "$test_file"
    perl -i -pe 's/\.assert_true\(/assert_true(/g;' "$test_file"
    perl -i -pe 's/\.assert_false\(/assert_false(/g;' "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有 clean_test 测试文件前缀移除完成！"