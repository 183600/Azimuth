#!/bin/bash
# 验证高级遥测系统测试用例

echo "验证新创建的高级遥测系统测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_advanced_telemetry_system_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_advanced_telemetry_system_tests.mbt"
else
    echo "✗ 测试文件不存在: azimuth_advanced_telemetry_system_tests.mbt"
    exit 1
fi

# 检查文件中是否包含10个测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_advanced_telemetry_system_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

if [ $test_count -eq 10 ]; then
    echo "✓ 测试用例数量符合要求 (10个)"
else
    echo "✗ 测试用例数量不符合要求 (需要10个，实际 $test_count 个)"
    exit 1
fi

# 检查是否已添加到moon.pkg.json
if grep -q "azimuth_advanced_telemetry_system_tests.mbt" /home/runner/work/Azimuth/Azimuth/src/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_advanced_telemetry_system_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！所有测试用例已成功创建并配置。"