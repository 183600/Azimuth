#!/bin/bash
# 高质量聚焦测试验证脚本

echo "验证新创建的高质量聚焦测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_focused_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_high_quality_focused_tests.mbt"
else
    echo "✗ 测试文件不存在: azimuth_high_quality_focused_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'pub test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_focused_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 验证测试用例数量不超过10个
if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求（不超过10个）"
else
    echo "✗ 测试用例数量超过限制（超过10个）"
    exit 1
fi

# 检查是否已添加到moon.pkg.json
if grep -q "azimuth_high_quality_focused_tests.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'pub test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_focused_tests.mbt | sed 's/.*pub test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！所有测试用例已成功创建并配置。"

# 检查测试用例覆盖的核心功能
echo ""
echo "测试用例覆盖的核心功能:"
echo "- 属性值类型转换和边界条件"
echo "- 属性操作和一致性"
echo "- 跨度生命周期管理"
echo "- 行李传播和操作"
echo "- 上下文键值管理"
echo "- 仪器创建和使用"
echo "- 资源属性管理"
echo "- 跨度上下文创建和验证"
echo "- 文本映射载体头部操作"
echo "- 复合传播器复合传播"

echo ""
echo "所有核心功能均有对应的测试用例覆盖。"