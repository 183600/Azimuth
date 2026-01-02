#!/bin/bash

echo "验证新创建的综合遥测系统测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth_comprehensive_telemetry_system_tests.mbt" ]; then
    echo "✓ 测试文件已成功创建"
    
    # 检查文件内容
    test_count=$(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth_comprehensive_telemetry_system_tests.mbt)
    echo "✓ 测试文件包含 $test_count 个测试用例"
    
    # 验证测试用例数量不超过10个
    if [ $test_count -le 10 ]; then
        echo "✓ 测试用例数量符合要求（不超过10个）"
    else
        echo "✗ 测试用例数量超过限制（$test_count > 10）"
        exit 1
    fi
    
    # 检查基本语法结构
    if grep -q "test \"" /home/runner/work/Azimuth/Azimuth/azimuth_comprehensive_telemetry_system_tests.mbt; then
        echo "✓ 测试用例语法结构正确"
    else
        echo "✗ 测试用例语法结构不正确"
        exit 1
    fi
    
    # 检查断言语句
    assert_count=$(grep -c "assert_" /home/runner/work/Azimuth/Azimuth/azimuth_comprehensive_telemetry_system_tests.mbt)
    if [ $assert_count -gt 0 ]; then
        echo "✓ 包含 $assert_count 个断言语句"
    else
        echo "✗ 未找到断言语句"
        exit 1
    fi
    
    echo ""
    echo "新创建的测试用例列表："
    grep "^test " /home/runner/work/Azimuth/Azimuth/azimuth_comprehensive_telemetry_system_tests.mbt | sed 's/^test "/- /' | sed 's/" {/ /'
    
    echo ""
    echo "✓ 所有验证通过！新的综合遥测系统测试文件已成功添加到项目中。"
else
    echo "✗ 测试文件未找到"
    exit 1
fi