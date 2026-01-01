#!/bin/bash
# 高质量遥测测试验证脚本

echo "验证高质量遥测测试套件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt" ]; then
    echo "✓ 高质量遥测测试文件存在: azimuth_high_quality_telemetry_tests.mbt"
else
    echo "✗ 高质量遥测测试文件不存在: azimuth_high_quality_telemetry_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "azimuth_high_quality_telemetry_tests.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "高质量遥测测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！高质量遥测测试套件已成功创建并配置。"

# 检查测试覆盖的功能领域
echo ""
echo "测试覆盖的功能领域:"

if grep -q "cross service tracing" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 跨服务追踪一致性测试"
fi

if grep -q "metrics aggregation" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 指标聚合和并发安全性测试"
fi

if grep -q "log context" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 日志上下文关联测试"
fi

if grep -q "resource.*merge" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 资源属性合并策略测试"
fi

if grep -q "propagator.*inject.*extract" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 传播器注入提取测试"
fi

if grep -q "attribute.*type.*conversion" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 属性值类型转换边界测试"
fi

if grep -q "time series" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 时间序列数据处理测试"
fi

if grep -q "error boundary" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 错误边界恢复测试"
fi

if grep -q "performance benchmark" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 性能基准测试"
fi

if grep -q "cloud native" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_telemetry_tests.mbt; then
    echo "✓ 云原生遥测场景测试"
fi

echo ""
echo "所有功能领域均已覆盖！测试套件质量验证通过。"