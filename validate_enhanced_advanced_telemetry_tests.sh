#!/bin/bash
# 验证增强高级遥测测试的脚本

echo "验证新创建的增强高级遥测测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_enhanced_advanced_telemetry_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_enhanced_advanced_telemetry_tests.mbt"
else
    echo "✗ 测试文件不存在: azimuth_enhanced_advanced_telemetry_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_enhanced_advanced_telemetry_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查测试用例数量不超过10个
if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求 (不超过10个)"
else
    echo "✗ 测试用例数量超过限制 ($test_count > 10)"
    exit 1
fi

# 检查基本的语法结构
if grep -q 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_enhanced_advanced_telemetry_tests.mbt; then
    echo "✓ 包含正确的测试结构"
else
    echo "✗ 缺少正确的测试结构"
    exit 1
fi

# 检查是否包含断言语句
assert_count=$(grep -c 'assert_' /home/runner/work/Azimuth/Azimuth/azimuth_enhanced_advanced_telemetry_tests.mbt)
if [ $assert_count -gt 0 ]; then
    echo "✓ 包含 $assert_count 个断言语句"
else
    echo "✗ 缺少断言语句"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_enhanced_advanced_telemetry_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！所有测试用例已成功创建。"