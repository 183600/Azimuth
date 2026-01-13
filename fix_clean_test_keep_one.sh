#!/bin/bash

# 只保留一个 clean_test 测试文件中的函数定义，删除其他文件中的重复定义

# 测试文件目录
TEST_DIR="src/clean_test/test"

# 保留函数定义的文件
KEEP_FILE="src/clean_test/test/simple_test.mbt"

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
    # 跳过非测试文件和保留文件
    if [[ ! -f "$test_file" ]] || [[ "$test_file" == "$KEEP_FILE" ]]; then
        continue
    fi
    
    echo "正在修复文件: $test_file"
    
    # 删除函数定义部分，只保留测试用例
    awk '/^test "/ { print_line = 1 } print_line == 1 { print }' "$test_file" > temp_file.mbt
    mv temp_file.mbt "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有 clean_test 测试文件重复定义删除完成！"