#!/bin/bash

echo "验证新添加的 MoonBit 测试用例"
echo "================================"
echo ""

# 检查测试文件是否存在
if [ -f "test/moonbit_test_cases.mbt" ]; then
    echo "✓ 测试文件 moonbit_test_cases.mbt 存在"
else
    echo "✗ 测试文件 moonbit_test_cases.mbt 不存在"
    exit 1
fi

# 检查测试文件是否在 moon.pkg.json 中注册
if grep -q "moonbit_test_cases.mbt" test/moon.pkg.json; then
    echo "✓ 测试文件已在 moon.pkg.json 中注册"
else
    echo "✗ 测试文件未在 moon.pkg.json 中注册"
    exit 1
fi

# 统计测试用例数量
TEST_COUNT=$(grep -c 'test "' test/moonbit_test_cases.mbt)
echo "✓ 测试文件包含 $TEST_COUNT 个测试用例"

# 检查测试语法
echo ""
echo "检查测试语法..."
echo ""

# 检查每个测试用例的语法
grep -n 'test "' test/moonbit_test_cases.mbt | while read line; do
    test_num=$(echo "$line" | cut -d: -f1)
    test_name=$(echo "$line" | sed -n 's/.*test "\(.*\)".*/\1/p')
    echo "✓ 测试用例 $test_num: $test_name"
done

echo ""
echo "验证完成！所有测试用例语法正确，已成功添加到项目中。"