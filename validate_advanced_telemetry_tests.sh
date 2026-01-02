#!/bin/bash

# 验证新创建的高级遥测测试用例

echo "验证 Azimuth 高级遥测系统测试用例"
echo "============================================"

# 检查测试文件是否存在
if [ -f "azimuth_advanced_telemetry_comprehensive_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_advanced_telemetry_comprehensive_tests.mbt"
else
    echo "✗ 测试文件不存在"
    exit 1
fi

# 统计测试用例数量
test_count=$(grep -c "^test " azimuth_advanced_telemetry_comprehensive_tests.mbt)
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
grep "^test " azimuth_advanced_telemetry_comprehensive_tests.mbt | sed 's/test "//' | sed 's/" {//'

# 检查文件中是否还有 azimuth:: 前缀
azimuth_prefix_count=$(grep -c "azimuth::" azimuth_advanced_telemetry_comprehensive_tests.mbt)
if [ $azimuth_prefix_count -eq 0 ]; then
    echo "✓ 所有函数调用已移除 azimuth:: 前缀"
else
    echo "✗ 仍有 $azimuth_prefix_count 个 azimuth:: 前缀未移除"
fi

# 检查测试覆盖的核心功能
echo ""
echo "测试覆盖的核心功能:"
if grep -q "time series data processing" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 时间序列数据处理测试"
fi
if grep -q "cross-service telemetry consistency" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 跨服务遥测一致性测试"
fi
if grep -q "dynamic configuration updates" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 动态配置更新测试"
fi
if grep -q "error boundary handling" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 错误边界处理测试"
fi
if grep -q "data serialization integrity" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 数据序列化完整性测试"
fi
if grep -q "platform-specific telemetry adaptation" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 平台特定遥测适配测试"
fi
if grep -q "advanced resource merge strategies" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 高级资源合并策略测试"
fi
if grep -q "high concurrency safety" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 高并发安全性测试"
fi
if grep -q "internationalization and globalization" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 国际化和全球化支持测试"
fi
if grep -q "real-time dashboard streaming" azimuth_advanced_telemetry_comprehensive_tests.mbt; then
    echo "✓ 实时仪表板数据流测试"
fi

# 检查 moon.pkg.json 是否包含新测试文件
if [ -f "azimuth/moon.pkg.json" ] && grep -q "azimuth_advanced_telemetry_comprehensive_tests.mbt" azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到 moon.pkg.json"
else
    echo "ℹ 测试文件未添加到 moon.pkg.json (可能需要手动添加)"
fi

echo ""
echo "验证完成！"
echo "已成功为 Azimuth 遥测系统添加了 $test_count 个高级测试用例。"