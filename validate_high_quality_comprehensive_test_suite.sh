#!/bin/bash
# 验证高质量综合测试套件

echo "验证高质量综合测试套件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_comprehensive_test_suite.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_high_quality_comprehensive_test_suite.mbt"
else
    echo "✗ 测试文件不存在: azimuth_high_quality_comprehensive_test_suite.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_comprehensive_test_suite.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "azimuth_high_quality_comprehensive_test_suite.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_comprehensive_test_suite.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！所有测试用例已成功创建并配置。"