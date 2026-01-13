#!/bin/bash

# 最终修复前缀问题

# 测试文件目录
TEST_DIR="src/azimuth/test"

# 遍历所有测试文件
for test_file in "$TEST_DIR"/*.mbt; do
    # 跳过非测试文件
    if [[ ! -f "$test_file" ]]; then
        continue
    fi
    
    echo "正在修复文件: $test_file"
    
    # 修复前缀问题
    perl -i -pe 's/@azimuthazimuth\./azimuth./g;' "$test_file"
    
    echo "已修复文件: $test_file"
done

echo "所有测试文件前缀修复完成！"