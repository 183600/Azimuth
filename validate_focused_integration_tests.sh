#!/bin/bash
# 验证新创建的集成测试文件

echo "验证新创建的集成测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_focused_integration_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_focused_integration_tests.mbt"
else
    echo "✗ 测试文件不存在: azimuth_focused_integration_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_focused_integration_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 验证测试用例数量不超过10个
if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求 (≤ 10)"
else
    echo "✗ 测试用例数量超过限制 (> 10)"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_focused_integration_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！所有测试用例已成功创建。"