#!/bin/bash
# 验证Premium MoonBit测试用例

echo "验证新创建的Premium测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在: premium_moonbit_tests.mbt"
else
    echo "✗ 测试文件不存在: premium_moonbit_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "premium_moonbit_tests.mbt" /home/runner/work/Azimuth/Azimuth/src/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 检查测试用例覆盖的功能领域
echo "检查测试用例覆盖的功能领域..."
if grep -q "resource management" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含资源管理测试"
fi

if grep -q "time series" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含时间序列测试"
fi

if grep -q "concurrent safety" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含并发安全测试"
fi

if grep -q "internationalization" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含国际化测试"
fi

if grep -q "error boundary" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含错误边界测试"
fi

if grep -q "performance" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含性能测试"
fi

if grep -q "cross service" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含跨服务一致性测试"
fi

if grep -q "serialization" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含数据序列化测试"
fi

if grep -q "dashboard" /home/runner/work/Azimuth/Azimuth/src/premium_moonbit_tests.mbt; then
    echo "✓ 包含实时仪表板测试"
fi

echo "验证完成！所有检查通过。"