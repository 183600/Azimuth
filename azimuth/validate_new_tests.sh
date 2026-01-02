#!/bin/bash
# 简单的验证脚本，检查新测试文件的语法

echo "验证新测试文件: azimuth_advanced_feature_comprehensive_tests.mbt"

# 检查文件是否存在
if [ ! -f "azimuth_advanced_feature_comprehensive_tests.mbt" ]; then
    echo "错误: 文件不存在"
    exit 1
fi

# 检查文件内容
echo "检查测试文件结构..."

# 检查是否包含8个测试用例
test_count=$(grep -c "^test " azimuth_advanced_feature_comprehensive_tests.mbt)
echo "测试用例数量: $test_count"

if [ $test_count -eq 8 ]; then
    echo "✓ 测试用例数量正确 (8个)"
else
    echo "✗ 测试用例数量不正确，期望8个，实际${test_count}个"
fi

# 检查测试用例名称
echo "检查测试用例名称..."
grep "^test " azimuth_advanced_feature_comprehensive_tests.mbt | while read line; do
    echo "  - $line"
done

# 检查基本语法结构
echo "检查基本语法结构..."
if grep -q "test " azimuth_advanced_feature_comprehensive_tests.mbt; then
    echo "✓ 包含测试定义"
else
    echo "✗ 缺少测试定义"
fi

if grep -q "assert_" azimuth_advanced_feature_comprehensive_tests.mbt; then
    echo "✓ 包含断言语句"
else
    echo "✗ 缺少断言语句"
fi

echo "验证完成"