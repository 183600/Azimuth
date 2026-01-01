#!/bin/bash
# 验证新创建的专业测试用例

echo "验证专业测试用例..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_specialized_test_cases.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_specialized_test_cases.mbt"
else
    echo "✗ 测试文件不存在: azimuth_specialized_test_cases.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_specialized_test_cases.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 验证测试用例数量不超过10个
if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求 (≤10个)"
else
    echo "✗ 测试用例数量过多 ($test_count > 10)"
    exit 1
fi

# 检查是否已添加到moon.pkg.json
if grep -q "azimuth_specialized_test_cases.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 显示测试用例列表
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_specialized_test_cases.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "✓ 所有验证通过!"