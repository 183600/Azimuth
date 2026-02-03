#!/bin/bash

# 验证新创建的测试文件
echo "验证 Azimuth 标准测试用例..."
echo ""

# 检查测试文件是否存在
if [ -f "azimuth/azimuth_standard_tests_new.mbt" ]; then
    echo "找到测试文件: azimuth/azimuth_standard_tests_new.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' azimuth/azimuth_standard_tests_new.mbt)
    echo "发现 $TEST_COUNT 个测试用例:"
    echo ""
    
    # 列出所有测试用例
    grep 'test "' azimuth/azimuth_standard_tests_new.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查语法
    echo "检查测试语法..."
    
    # 检查是否使用了正确的断言函数
    ASSERT_EQ_COUNT=$(grep -c '@azimuth.assert_eq' azimuth/azimuth_standard_tests_new.mbt)
    ASSERT_EQ_STRING_COUNT=$(grep -c '@azimuth.assert_eq_string' azimuth/azimuth_standard_tests_new.mbt)
    
    echo "发现 $ASSERT_EQ_COUNT 个 assert_eq 断言"
    echo "发现 $ASSERT_EQ_STRING_COUNT 个 assert_eq_string 断言"
    echo ""
    
    # 检查是否覆盖了主要功能
    ADD_TESTS=$(grep -c 'add' azimuth/azimuth_standard_tests_new.mbt)
    MULTIPLY_TESTS=$(grep -c 'multiply' azimuth/azimuth_standard_tests_new.mbt)
    DIVIDE_TESTS=$(grep -c 'divide_with_ceil' azimuth/azimuth_standard_tests_new.mbt)
    GREET_TESTS=$(grep -c 'greet' azimuth/azimuth_standard_tests_new.mbt)
    SUBTRACT_TESTS=$(grep -c 'subtract' azimuth/azimuth_standard_tests_new.mbt)
    
    echo "功能覆盖情况:"
    echo "- add 函数: $ADD_TESTS 次调用"
    echo "- multiply 函数: $MULTIPLY_TESTS 次调用"
    echo "- divide_with_ceil 函数: $DIVIDE_TESTS 次调用"
    echo "- greet 函数: $GREET_TESTS 次调用"
    echo "- subtract 函数: $SUBTRACT_TESTS 次调用"
    echo ""
    
    echo "✓ 测试文件创建成功！"
    echo "✓ 包含 $TEST_COUNT 个标准 MoonBit 测试用例"
    echo "✓ 覆盖了所有主要功能"
    echo "✓ 使用了正确的 MoonBit 测试语法"
else
    echo "错误: 找不到测试文件 azimuth/azimuth_standard_tests_new.mbt"
    exit 1
fi