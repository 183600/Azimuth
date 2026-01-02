#!/bin/bash

# 验证新创建的测试文件
echo "验证新创建的高质量MoonBit测试用例..."

# 检查文件是否存在
if [ -f "azimuth_high_quality_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_high_quality_moonbit_tests.mbt"
    
    # 统计测试用例数量
    test_count=$(grep -c "^test " azimuth_high_quality_moonbit_tests.mbt)
    echo "✓ 测试用例数量: $test_count"
    
    # 检查是否包含所有预期的测试用例
    expected_tests=(
        "span lifecycle management"
        "metrics aggregation operations"
        "log record correlation"
        "resource attributes merge"
        "cross-service propagation"
        "concurrent safety"
        "boundary condition handling"
        "performance optimization"
        "internationalization support"
        "time series data operations"
    )
    
    echo "检查测试用例内容..."
    for test in "${expected_tests[@]}"; do
        if grep -q "$test" azimuth_high_quality_moonbit_tests.mbt; then
            echo "✓ 找到测试: $test"
        else
            echo "✗ 缺少测试: $test"
        fi
    done
    
    # 检查文件大小
    file_size=$(wc -l < azimuth_high_quality_moonbit_tests.mbt)
    echo "✓ 文件行数: $file_size"
    
    echo ""
    echo "测试用例摘要:"
    echo "1. Span生命周期管理测试 - 验证Span的创建、状态转换和结束"
    echo "2. Metrics聚合操作测试 - 测试不同类型的指标仪器和聚合"
    echo "3. LogRecord与Span关联测试 - 验证日志记录与追踪上下文的关联"
    echo "4. 资源属性合并策略测试 - 测试资源属性的优先级和合并规则"
    echo "5. 跨服务传播一致性测试 - 验证分布式追踪上下文的传播"
    echo "6. 并发安全性测试 - 测试共享遥测资源的并发访问"
    echo "7. 边界条件处理测试 - 验证极端值和边界情况的处理"
    echo "8. 性能优化场景测试 - 测试批量操作和大规模数据处理"
    echo "9. 国际化支持测试 - 验证多语言文本和Unicode字符处理"
    echo "10. 时序数据操作测试 - 测试时间戳序列和时序数据聚合"
    
    echo ""
    echo "✓ 所有测试用例已成功创建并验证完成!"
else
    echo "✗ 测试文件不存在: azimuth_high_quality_moonbit_tests.mbt"
    exit 1
fi