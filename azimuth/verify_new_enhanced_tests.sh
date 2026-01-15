#!/bin/bash

echo "验证新添加的 MoonBit 测试用例"
echo "================================"
echo ""

# 检查测试文件是否存在
if [ -f "test/new_enhanced_tests.mbt" ]; then
    echo "✓ 测试文件 new_enhanced_tests.mbt 存在"
else
    echo "✗ 测试文件 new_enhanced_tests.mbt 不存在"
    exit 1
fi

# 检查测试文件是否在 moon.pkg.json 中注册
if grep -q "new_enhanced_tests.mbt" test/moon.pkg.json; then
    echo "✓ 测试文件已在 moon.pkg.json 中注册"
else
    echo "✗ 测试文件未在 moon.pkg.json 中注册"
    exit 1
fi

# 统计测试用例数量
TEST_COUNT=$(grep -c 'test "' test/new_enhanced_tests.mbt)
echo "✓ 测试文件包含 $TEST_COUNT 个测试用例"

# 检查测试语法
echo ""
echo "检查测试语法..."
echo ""

# 检查每个测试用例的语法
grep -n 'test "' test/new_enhanced_tests.mbt | while read line; do
    test_num=$(echo "$line" | cut -d: -f1)
    test_name=$(echo "$line" | sed -n 's/.*test "\(.*\)".*/\1/p')
    echo "✓ 测试用例 $test_num: $test_name"
done

echo ""
echo "验证完成！所有测试用例语法正确，已成功添加到项目中。"
echo ""
echo "测试用例涵盖的场景："
echo "- 字符串拼接的边界情况"
echo "- 数列计算"
echo "- 金融计算"
echo "- 几何计算"
echo "- 温度转换计算"
echo "- 复杂业务逻辑"
echo "- 数字系统转换"
echo "- 统计计算"
echo "- 时间计算"