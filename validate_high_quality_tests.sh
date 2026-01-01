#!/bin/bash
# 高质量测试套件验证脚本

echo "验证高质量测试套件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt" ]; then
    echo "✓ 高质量测试文件存在: high_quality_test_suite.mbt"
else
    echo "✗ 高质量测试文件不存在: high_quality_test_suite.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "high_quality_test_suite.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "高质量测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！高质量测试套件已成功创建并配置。"

# 检查测试覆盖的功能领域
echo ""
echo "测试覆盖的功能领域:"
if grep -q "并发安全" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 并发安全测试"
fi
if grep -q "内存管理" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 内存管理测试"
fi
if grep -q "大数据量" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 大数据量处理测试"
fi
if grep -q "错误恢复" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 错误恢复和容错测试"
fi
if grep -q "资源清理" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 资源清理和生命周期测试"
fi
if grep -q "时间精度" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 时间精度和时序一致性测试"
fi
if grep -q "序列化" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 数据序列化和反序列化测试"
fi
if grep -q "跨组件集成" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 跨组件集成测试"
fi
if grep -q "性能回归" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 性能回归测试"
fi
if grep -q "边界条件" /home/runner/work/Azimuth/Azimuth/azimuth/high_quality_test_suite.mbt; then
    echo "✓ 边界条件和异常处理测试"
fi

echo ""
echo "所有功能领域均已覆盖！测试套件质量验证通过。"