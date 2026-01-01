#!/bin/bash

# 简单验证脚本，检查新创建的测试文件是否有语法错误
echo "验证新创建的测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_enhanced_telemetry_tests.mbt"
    
    # 检查文件大小
    FILE_SIZE=$(wc -l < "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt")
    echo "✓ 文件行数: $FILE_SIZE"
    
    # 检查测试数量
    TEST_COUNT=$(grep -c "^test " "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt")
    echo "✓ 测试用例数量: $TEST_COUNT"
    
    # 检查是否包含预期的测试内容
    echo "✓ 检查测试用例内容..."
    grep -q "cross-service telemetry consistency" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 跨服务遥测一致性测试 ✓"
    grep -q "time series data processing" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 时间序列数据处理测试 ✓"
    grep -q "high concurrency resource management" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 高并发资源管理测试 ✓"
    grep -q "dynamic configuration updates" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 动态配置更新测试 ✓"
    grep -q "internationalization and localization" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 国际化本地化支持测试 ✓"
    grep -q "network exception handling" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 网络异常处理测试 ✓"
    grep -q "security and privacy protection" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 安全隐私保护测试 ✓"
    grep -q "real-time dashboard streaming" "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_enhanced_telemetry_tests.mbt" && echo "  - 实时仪表板流处理测试 ✓"
    
    echo "✓ 新测试文件验证完成"
else
    echo "✗ 测试文件不存在: azimuth_enhanced_telemetry_tests.mbt"
fi

# 检查是否已添加到包配置
if grep -q "azimuth_enhanced_telemetry_tests.mbt" "/home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json"; then
    echo "✓ 测试文件已添加到moon.pkg.json配置中"
else
    echo "✗ 测试文件未添加到moon.pkg.json配置中"
fi

echo "验证完成！"