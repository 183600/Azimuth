#!/bin/bash

# 验证新创建的测试文件
echo "验证新创建的测试文件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_test_suite.mbt" ]; then
    echo "✓ 测试文件存在"
    
    # 检查文件内容
    echo "✓ 检查测试文件内容..."
    grep -c "test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_test_suite.mbt
    
    # 列出所有测试用例
    echo "✓ 列出所有测试用例:"
    grep "test \"" /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_new_comprehensive_test_suite.mbt | sed 's/.*test "\([^"]*\)".*/\1/'
    
    # 检查是否已添加到配置文件
    echo "✓ 检查是否已添加到配置文件..."
    if grep -q "azimuth_new_comprehensive_test_suite.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
        echo "✓ 已添加到moon.pkg.json"
    else
        echo "✗ 未添加到moon.pkg.json"
    fi
    
    echo "✓ 验证完成"
else
    echo "✗ 测试文件不存在"
fi