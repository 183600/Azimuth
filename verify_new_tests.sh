#!/bin/bash

# 验证新创建的测试文件
echo "验证新创建的测试文件..."

# 定义测试文件列表
test_files=(
    "azimuth_distributed_tracing_consistency_tests.mbt"
    "azimuth_memory_management_resource_leak_tests.mbt"
    "azimuth_performance_stress_benchmark_tests.mbt"
    "azimuth_security_privacy_protection_tests.mbt"
    "azimuth_multi_tenant_isolation_tests.mbt"
    "azimuth_edge_iot_device_telemetry_tests.mbt"
    "azimuth_timeseries_compression_storage_tests.mbt"
    "azimuth_dynamic_sampling_strategy_tests.mbt"
)

# 验证每个文件
for file in "${test_files[@]}"; do
    echo "验证文件: $file"
    
    # 检查文件是否存在
    if [ -f "$file" ]; then
        echo "✓ 文件存在"
        
        # 检查文件大小
        size=$(stat -c%s "$file")
        echo "✓ 文件大小: $size 字节"
        
        # 检查测试用例数量
        test_count=$(grep -c "test \"" "$file")
        echo "✓ 测试用例数量: $test_count"
        
        # 检查基本语法结构
        if grep -q "test \"" "$file" && grep -q "{" "$file" && grep -q "}" "$file"; then
            echo "✓ 基本语法结构正确"
        else
            echo "✗ 基本语法结构可能有问题"
        fi
        
        # 检查是否有assert语句
        assert_count=$(grep -c "assert_" "$file")
        echo "✓ 断言数量: $assert_count"
        
    else
        echo "✗ 文件不存在"
    fi
    
    echo "---"
done

echo "验证完成！"