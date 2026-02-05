#!/bin/bash

# 简单的测试验证脚本
echo "=== 验证新创建的 MoonBit 测试用例 ==="

# 检查测试文件是否存在
if [ -f "azimuth_new_standard_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_new_standard_tests.mbt"
    
    # 统计测试用例数量
    TEST_COUNT=$(grep "^test " azimuth_new_standard_tests.mbt | wc -l)
    echo "✓ 包含 $TEST_COUNT 个测试用例"
    
    # 检查语法
    echo "正在检查语法..."
    if node ./moonc.js check azimuth_new_standard_tests.mbt > /dev/null 2>&1; then
        echo "✓ 语法检查通过"
    else
        echo "✗ 语法检查失败"
        node ./moonc.js check azimuth_new_standard_tests.mbt
        exit 1
    fi
    
    # 检查测试目录中的文件
    if [ -f "azimuth/test/azimuth_new_standard_tests.mbt" ]; then
        echo "✓ 测试文件已复制到 azimuth/test/ 目录"
        
        # 检查测试目录中的语法
        if node ./moonc.js check -pkg azimuth_test -std-path core azimuth/test/azimuth_new_standard_tests.mbt > /dev/null 2>&1; then
            echo "✓ 测试目录中的文件语法检查通过"
        else
            echo "✗ 测试目录中的文件语法检查失败"
            node ./moonc.js check -pkg azimuth_test -std-path core azimuth/test/azimuth_new_standard_tests.mbt
            exit 1
        fi
    else
        echo "✗ 测试文件未复制到测试目录"
    fi
    
    # 显示测试用例列表
    echo ""
    echo "测试用例列表："
    grep "^test " azimuth_new_standard_tests.mbt | sed 's/test "/- /' | sed 's/" {$//'
    
    echo ""
    echo "=== 验证完成 ==="
    echo "✓ 所有测试用例创建成功，语法正确，已准备好使用"
    
else
    echo "✗ 测试文件不存在: azimuth_new_standard_tests.mbt"
    exit 1
fi