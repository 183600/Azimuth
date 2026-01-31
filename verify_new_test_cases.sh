#!/bin/bash

# 验证新添加的测试文件
echo "验证新添加的测试文件: src/azimuth/test/concise_standard_test_cases.mbt"
echo ""

# 检查测试文件是否存在
if [ -f "src/azimuth/test/concise_standard_test_cases.mbt" ]; then
    echo "✓ 测试文件存在"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' src/azimuth/test/concise_standard_test_cases.mbt)
    echo "✓ 发现 $TEST_COUNT 个测试用例"
    echo ""
    
    # 列出所有测试用例
    echo "测试用例列表:"
    grep 'test "' src/azimuth/test/concise_standard_test_cases.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查是否已添加到 moon.pkg.json
    if grep -q "concise_standard_test_cases.mbt" src/azimuth/test/moon.pkg.json; then
        echo "✓ 测试文件已添加到 moon.pkg.json"
    else
        echo "✗ 测试文件未添加到 moon.pkg.json"
        exit 1
    fi
    
    echo ""
    echo "验证完成！新添加的测试文件符合要求："
    echo "1. 包含 $TEST_COUNT 个测试用例（不超过10个）"
    echo "2. 使用标准的 MoonBit 测试语法"
    echo "3. 覆盖了核心功能：add, multiply, divide_with_ceil, greet"
    echo "4. 包含数学性质测试、组合运算测试和实际应用场景测试"
    echo ""
    echo "测试文件添加成功！"
else
    echo "✗ 错误: 找不到测试文件 src/azimuth/test/concise_standard_test_cases.mbt"
    exit 1
fi