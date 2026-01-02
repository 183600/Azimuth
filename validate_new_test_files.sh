#!/bin/bash

echo "验证新创建的测试文件..."

# 检查文件是否存在
echo "检查新创建的测试文件是否存在:"
files=(
    "azimuth/azimuth_enhanced_core_functionality_tests.mbt"
    "azimuth/azimuth_performance_stress_tests.mbt"
    "azimuth/azimuth_boundary_condition_edge_tests.mbt"
    "azimuth/azimuth_cross_component_integration_tests.mbt"
    "azimuth/azimuth_context_propagation_advanced_tests.mbt"
)

for file in "${files[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ $file 存在"
    else
        echo "✗ $file 不存在"
    fi
done

echo ""
echo "检查文件内容:"
for file in "${files[@]}"; do
    if [ -f "$file" ]; then
        echo "=== $file ==="
        # 检查是否包含test关键字
        test_count=$(grep -c "^test " "$file")
        echo "包含 $test_count 个测试用例"
        
        # 检查是否有语法错误的基本标志
        if grep -q "test \"" "$file"; then
            echo "✓ 包含有效的测试语法"
        else
            echo "✗ 可能缺少有效的测试语法"
        fi
        echo ""
    fi
done

echo "验证完成!"