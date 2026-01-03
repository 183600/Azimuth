#!/bin/bash
# 验证综合高质量遥测测试脚本

echo "验证新创建的综合高质量遥测测试文件..."

# 检查文件是否存在
test_file="/home/runner/work/Azimuth/Azimuth/azimuth_comprehensive_high_quality_telemetry_tests.mbt"
if [ -f "$test_file" ]; then
    echo "✓ 测试文件存在: azimuth_comprehensive_high_quality_telemetry_tests.mbt"
else
    echo "✗ 测试文件不存在: azimuth_comprehensive_high_quality_telemetry_tests.mbt"
    exit 1
fi

# 检查文件中是否包含测试用例
test_count=$(grep -c 'test "' "$test_file")
echo "✓ 找到 $test_count 个测试用例"

# 验证测试用例数量不超过10个
if [ "$test_count" -le 10 ]; then
    echo "✓ 测试用例数量符合要求 (不超过10个)"
else
    echo "✗ 测试用例数量超过10个"
    exit 1
fi

# 检查测试文件中是否包含基本的MoonBit语法元素
if grep -q 'test "' "$test_file"; then
    echo "✓ 包含有效的测试语法"
else
    echo "✗ 缺少有效的测试语法"
    exit 1
fi

# 检查是否包含断言语句
assert_count=$(grep -c 'assert_' "$test_file")
if [ "$assert_count" -gt 0 ]; then
    echo "✓ 包含 $assert_count 个断言语句"
else
    echo "✗ 缺少断言语句"
    exit 1
fi

# 列出所有测试用例名称
echo ""
echo "测试用例列表:"
grep 'test "' "$test_file" | sed 's/.*test "\([^"]*\)".*/\1/'

# 检查测试内容覆盖率
test_functions=(
    "遥测数据聚合与分析"
    "分布式追踪完整性验证"
    "性能监控与资源使用优化"
    "错误处理与自动恢复机制"
    "数据完整性与一致性验证"
    "自定义度量与高级分析"
    "资源监控与自适应调整"
    "智能采样策略与数据保留"
    "配置管理与动态更新"
    "多格式数据导出与集成"
)

echo ""
echo "验证测试内容覆盖率:"
for func in "${test_functions[@]}"; do
    if grep -q "$func" "$test_file"; then
        echo "✓ 包含测试: $func"
    else
        echo "✗ 缺少测试: $func"
    fi
done

# 检查文件大小（确保不是空文件）
file_size=$(stat -c%s "$test_file")
if [ "$file_size" -gt 1000 ]; then
    echo "✓ 测试文件大小合理 ($file_size 字节)"
else
    echo "✗ 测试文件过小 ($file_size 字节)"
    exit 1
fi

echo ""
echo "验证完成！综合高质量遥测测试已成功创建并通过验证。"