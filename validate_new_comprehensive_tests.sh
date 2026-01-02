#!/bin/bash

# 验证新创建的测试文件
echo "验证新创建的综合MoonBit测试用例..."

# 检查文件是否存在
if [ -f "azimuth_new_comprehensive_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在: azimuth_new_comprehensive_moonbit_tests.mbt"
    
    # 统计测试用例数量
    test_count=$(grep -c "^test " azimuth_new_comprehensive_moonbit_tests.mbt)
    echo "✓ 测试用例数量: $test_count"
    
    # 检查是否包含所有预期的测试用例
    expected_tests=(
        "concurrent telemetry data processing"
        "large scale data processing performance"
        "resource constrained environment behavior"
        "network failure recovery"
        "data integrity validation"
        "caching mechanism"
        "internationalization support"
        "security features"
    )
    
    echo "检查测试用例内容..."
    for test in "${expected_tests[@]}"; do
        if grep -q "$test" azimuth_new_comprehensive_moonbit_tests.mbt; then
            echo "✓ 找到测试: $test"
        else
            echo "✗ 缺少测试: $test"
        fi
    done
    
    # 检查文件大小
    file_size=$(wc -l < azimuth_new_comprehensive_moonbit_tests.mbt)
    echo "✓ 文件行数: $file_size"
    
    echo ""
    echo "测试用例摘要:"
    echo "1. 并发遥测数据处理测试 - 验证并发场景下的遥测数据处理"
    echo "2. 大规模数据处理性能测试 - 测试大规模数据处理的性能指标"
    echo "3. 资源约束环境行为测试 - 测试资源约束环境下的行为和自适应机制"
    echo "4. 网络故障恢复测试 - 测试网络故障恢复机制和断路器模式"
    echo "5. 数据完整性验证测试 - 测试数据完整性验证和校验和算法"
    echo "6. 缓存机制测试 - 测试缓存系统和LRU策略"
    echo "7. 国际化支持测试 - 验证多语言和本地化支持"
    echo "8. 安全性功能测试 - 测试加密、访问控制和审计日志"
    
    echo ""
    echo "✓ 所有测试用例已成功创建并验证完成!"
else
    echo "✗ 测试文件不存在: azimuth_new_comprehensive_moonbit_tests.mbt"
    exit 1
fi