#!/bin/bash

# 验证新创建的高质量测试用例

echo "验证 Azimuth 遥感测量系统的高质量测试用例"
echo "============================================"

# 检查测试文件是否存在
if [ -f "azimuth_high_quality_comprehensive_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_high_quality_comprehensive_tests.mbt"
else
    echo "✗ 测试文件不存在"
    exit 1
fi

# 统计测试用例数量
test_count=$(grep -c "^test " azimuth_high_quality_comprehensive_tests.mbt)
echo "✓ 测试用例数量: $test_count"

if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求 (不超过10个)"
else
    echo "✗ 测试用例数量超过要求"
    exit 1
fi

# 检查测试用例名称
echo ""
echo "测试用例列表:"
grep "^test " azimuth_high_quality_comprehensive_tests.mbt | sed 's/test "//' | sed 's/" {//'

# 检查文件中是否还有 azimuth:: 前缀
azimuth_prefix_count=$(grep -c "azimuth::" azimuth_high_quality_comprehensive_tests.mbt)
if [ $azimuth_prefix_count -eq 0 ]; then
    echo "✓ 所有函数调用已移除 azimuth:: 前缀"
else
    echo "✗ 仍有 $azimuth_prefix_count 个 azimuth:: 前缀未移除"
fi

# 检查测试覆盖的核心功能
echo ""
echo "测试覆盖的核心功能:"
if grep -q "context_propagation_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 上下文传播测试"
fi
if grep -q "tracing_consistency_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 跨服务追踪一致性测试"
fi
if grep -q "metrics_aggregation_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 度量系统聚合操作测试"
fi
if grep -q "log_record_trace_correlation_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 日志记录与追踪关联测试"
fi
if grep -q "resource_merge_strategy_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 资源合并策略测试"
fi
if grep -q "concurrent_safety_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 并发安全性测试"
fi
if grep -q "serialization_integrity_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 序列化完整性测试"
fi
if grep -q "boundary_conditions_error_recovery_test" azimuth_high_quality_comprehensive_tests.mbt; then
    echo "✓ 边界条件和错误恢复测试"
fi

# 检查 moon.pkg.json 是否包含新测试文件
if grep -q "azimuth_high_quality_comprehensive_tests.mbt" azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到 moon.pkg.json"
else
    echo "✗ 测试文件未添加到 moon.pkg.json"
fi

echo ""
echo "验证完成！"
echo "已成功为 Azimuth 遥感测量系统添加了 $test_count 个高质量测试用例。"