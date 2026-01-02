#!/bin/bash
echo "验证新创建的MoonBit测试文件..."

# 检查文件是否存在
if [ -f "azimuth_core_focused_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件已创建"
    
    # 检查文件行数
    lines=$(wc -l < azimuth_core_focused_moonbit_tests.mbt)
    echo "✓ 文件包含 $lines 行代码"
    
    # 检查测试用例数量
    tests=$(grep -c "test \"" azimuth_core_focused_moonbit_tests.mbt)
    echo "✓ 包含 $tests 个测试用例"
    
    # 显示测试用例名称
    echo ""
    echo "测试用例列表："
    grep "test \"" azimuth_core_focused_moonbit_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'
    
    echo ""
    echo "✓ 所有测试用例已成功创建"
else
    echo "✗ 测试文件未找到"
    exit 1
fi