#!/bin/bash

echo "=== 验证标准 MoonBit 测试用例 ==="

cd /home/runner/work/Azimuth/Azimuth/src/azimuth

# 检查测试文件是否存在
if [ -f "standard_moonbit_test_cases.mbt" ]; then
    echo "✓ 测试文件存在: standard_moonbit_test_cases.mbt"
    
    echo ""
    echo "显示测试文件的前20行:"
    head -20 standard_moonbit_test_cases.mbt
    
    echo ""
    echo "统计测试用例数量:"
    test_count=$(grep -c "^test " standard_moonbit_test_cases.mbt)
    echo "测试用例数量: $test_count"
    
    echo ""
    echo "显示所有测试用例名称:"
    grep "^test " standard_moonbit_test_cases.mbt
    
    echo ""
    echo "验证测试用例语法结构..."
    
    # 检查基本的测试语法
    if grep -q "^test " standard_moonbit_test_cases.mbt; then
        echo "✓ 找到测试用例定义"
    else
        echo "✗ 没有找到测试用例定义"
    fi
    
    if grep -q "assert_eq" standard_moonbit_test_cases.mbt; then
        echo "✓ 找到断言语句"
    else
        echo "✗ 没有找到断言语句"
    fi
    
    if grep -q "assert_eq_string" standard_moonbit_test_cases.mbt; then
        echo "✓ 找到字符串断言语句"
    else
        echo "✗ 没有找到字符串断言语句"
    fi
    
    # 验证测试用例数量不超过10个
    if [ "$test_count" -le 10 ]; then
        echo "✓ 测试用例数量符合要求 (≤ 10个)"
    else
        echo "✗ 测试用例数量超出要求 (> 10个)"
    fi
    
    echo ""
    echo "=== 验证完成 ==="
else
    echo "✗ 测试文件不存在: standard_moonbit_test_cases.mbt"
fi