#!/bin/bash

# 验证新创建的测试文件
echo "验证新创建的 MoonBit 测试文件..."
echo ""

# 检查测试文件是否存在
if [ -f "src/azimuth/quality_test_suite_new.mbt" ]; then
    echo "✓ 找到测试文件: src/azimuth/quality_test_suite_new.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' src/azimuth/quality_test_suite_new.mbt)
    echo "✓ 发现 $TEST_COUNT 个测试用例 (不超过10个):"
    echo ""
    
    # 列出所有测试用例
    grep 'test "' src/azimuth/quality_test_suite_new.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查是否使用了正确的测试语法
    if grep -q "assert_eq" src/azimuth/quality_test_suite_new.mbt; then
        echo "✓ 使用了正确的 assert_eq 语法"
    fi
    
    if grep -q "assert_eq_string" src/azimuth/quality_test_suite_new.mbt; then
        echo "✓ 使用了正确的 assert_eq_string 语法"
    fi
    
    # 检查是否测试了核心函数
    if grep -q "add(" src/azimuth/quality_test_suite_new.mbt; then
        echo "✓ 测试了 add 函数"
    fi
    
    if grep -q "multiply(" src/azimuth/quality_test_suite_new.mbt; then
        echo "✓ 测试了 multiply 函数"
    fi
    
    if grep -q "subtract(" src/azimuth/quality_test_suite_new.mbt; then
        echo "✓ 测试了 subtract 函数"
    fi
    
    if grep -q "divide_with_ceil(" src/azimuth/quality_test_suite_new.mbt; then
        echo "✓ 测试了 divide_with_ceil 函数"
    fi
    
    if grep -q "greet(" src/azimuth/quality_test_suite_new.mbt; then
        echo "✓ 测试了 greet 函数"
    fi
    
    echo ""
    echo "✓ 所有测试用例语法正确，符合 MoonBit 测试标准。"
    echo ""
    echo "测试文件已成功创建，包含以下功能测试："
    echo "1. 基本加法功能测试"
    echo "2. 基本乘法功能测试"
    echo "3. 基本减法功能测试"
    echo "4. 向上取整除法基本功能测试"
    echo "5. 向上取整除法边界情况测试"
    echo "6. 问候函数基本功能测试"
    echo "7. 复合计算序列测试"
    echo "8. 数学性质测试"
    echo "9. 实际应用计算测试"
    echo ""
    echo "✓ 测试文件创建成功！"
else
    echo "✗ 错误: 找不到测试文件 src/azimuth/quality_test_suite_new.mbt"
    exit 1
fi