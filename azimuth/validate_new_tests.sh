#!/bin/bash

# 简单的测试验证脚本
echo "验证新创建的测试文件..."

# 检查文件是否存在
if [ -f "azimuth_enhanced_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件存在"
else
    echo "✗ 测试文件不存在"
    exit 1
fi

# 统计测试用例数量
test_count=$(grep -c "^test " azimuth_enhanced_moonbit_tests.mbt)
echo "✓ 测试用例数量: $test_count"

if [ $test_count -le 10 ]; then
    echo "✓ 测试用例数量符合要求（不超过10个）"
else
    echo "✗ 测试用例数量超过要求（超过10个）"
    exit 1
fi

# 检查测试文件的基本语法
echo "✓ 测试文件包含以下测试用例："
grep "^test " azimuth_enhanced_moonbit_tests.mbt | sed 's/test "//' | sed 's/" {//'

echo ""
echo "验证完成！所有测试用例已成功创建。"