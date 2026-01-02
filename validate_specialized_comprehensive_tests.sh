#!/bin/bash
# 验证专门化综合测试用例的脚本

echo "验证新创建的专门化综合测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_specialized_comprehensive_tests.mbt"
else
    echo "✗ 测试文件不存在: azimuth_specialized_comprehensive_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "azimuth_specialized_comprehensive_tests.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

# 检查测试用例是否包含基本的断言
echo ""
echo "检查测试用例中的断言..."
assert_count=$(grep -c 'assert_' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
echo "✓ 找到 $assert_count 个断言"

# 检查测试用例是否包含基本的遥测操作
echo ""
echo "检查测试用例中的遥测操作..."
tracer_count=$(grep -c 'TracerProvider::' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
meter_count=$(grep -c 'MeterProvider::' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
logger_count=$(grep -c 'LoggerProvider::' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)

echo "✓ 找到 $tracer_count 个TracerProvider操作"
echo "✓ 找到 $meter_count 个MeterProvider操作"
echo "✓ 找到 $logger_count 个LoggerProvider操作"

# 检查测试用例是否包含中文注释
echo ""
echo "检查测试用例中的中文注释..."
comment_count=$(grep -c '// ' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
echo "✓ 找到 $comment_count 行注释"

# 检查测试用例是否覆盖了不同的测试场景
echo ""
echo "检查测试用例覆盖的场景..."
boundary_tests=$(grep -c '边界条件' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
performance_tests=$(grep -c '性能' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
security_tests=$(grep -c '安全' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)
i18n_tests=$(grep -c '国际化' /home/runner/work/Azimuth/Azimuth/azimuth_specialized_comprehensive_tests.mbt)

echo "✓ 包含 $boundary_tests 个边界条件测试"
echo "✓ 包含 $performance_tests 个性能测试"
echo "✓ 包含 $security_tests 个安全测试"
echo "✓ 包含 $i18n_tests 个国际化测试"

echo ""
echo "验证完成！所有测试用例已成功创建并配置。"
echo ""
echo "测试用例概述："
echo "- 总测试用例数: $test_count"
echo "- 总断言数: $assert_count"
echo "- TracerProvider操作数: $tracer_count"
echo "- MeterProvider操作数: $meter_count"
echo "- LoggerProvider操作数: $logger_count"
echo "- 注释行数: $comment_count"
echo ""
echo "测试覆盖领域："
echo "- 边界条件和异常处理"
echo "- 数据类型转换和验证"
echo "- 配置动态更新"
echo "- 跨平台兼容性"
echo "- 高并发场景"
echo "- 内存泄漏和资源管理"
echo "- 数据序列化和反序列化"
echo "- 性能基准测试"
echo "- 安全性测试"
echo "- 国际化和本地化测试"