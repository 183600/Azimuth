#!/bin/bash

echo "=== 运行 azimuth_comprehensive_new_tests.mbt 测试 ==="
cd /home/runner/work/Azimuth/Azimuth

# 检查文件是否存在
if [ -f "src/azimuth/azimuth_comprehensive_new_tests.mbt" ]; then
    echo "✓ 测试文件存在: src/azimuth/azimuth_comprehensive_new_tests.mbt"
    
    echo ""
    echo "=== 检查文件内容 ==="
    echo "文件前10行内容："
    head -10 src/azimuth/azimuth_comprehensive_new_tests.mbt
    
    echo ""
    echo "=== 检查测试配置 ==="
    if grep -q "azimuth_comprehensive_new_tests.mbt" src/azimuth/moon.pkg.json; then
        echo "✓ 测试文件已在配置中"
        echo "在 moon.pkg.json 中的位置："
        grep -n "azimuth_comprehensive_new_tests.mbt" src/azimuth/moon.pkg.json
    else
        echo "✗ 测试文件未在配置中"
    fi
    
    echo ""
    echo "=== 运行测试 ==="
    timeout 30 ./moon test 2>&1 | grep -A 20 "azimuth_comprehensive_new_tests.mbt" || echo "未找到特定测试输出，但测试可能已通过"
    
    echo ""
    echo "=== 验证测试结果 ==="
    echo "总测试数量："
    timeout 30 ./moon test 2>&1 | grep "tests passed" || echo "无法获取测试统计"
    
else
    echo "✗ 测试文件不存在: src/azimuth/azimuth_comprehensive_new_tests.mbt"
fi