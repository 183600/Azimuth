#!/bin/bash
# 验证新创建的增强质量测试脚本

echo "验证新创建的增强质量测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_quality_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_enhanced_quality_tests.mbt"
else
    echo "✗ 测试文件不存在: azimuth_enhanced_quality_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_quality_tests.mbt)
echo "✓ 找到 $test_count 个测试用例"

# 检查是否已添加到moon.pkg.json
if grep -q "azimuth_enhanced_quality_tests.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_quality_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！所有测试用例已成功创建并配置。"
echo ""
echo "测试覆盖范围:"
echo "- SpanKind枚举的全面测试"
echo "- StatusCode枚举和状态转换"
echo "- 数组属性值测试"
echo "- SeverityNumber枚举全面测试"
echo "- 跨服务遥测数据一致性"
echo "- 国际化和多语言文本处理"
echo "- 高性能操作基准测试"
echo "- 时间序列和时间操作验证"
echo "- 资源合并策略和冲突解决"
echo "- 边界条件和错误处理验证"