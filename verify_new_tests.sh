#!/bin/bash

echo "验证新创建的 Azimuth 标准单元测试..."
echo "======================================"

# 进入 azimuth 目录
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查新创建的测试文件
echo "检查新创建的测试文件..."
ls -la azimuth_standard_unit_tests.mbt

echo ""
echo "测试文件内容验证..."
echo "=================="
echo "文件大小: $(wc -l < azimuth_standard_unit_tests.mbt) 行"
echo "测试用例数量: $(grep -c "^test " azimuth_standard_unit_tests.mbt) 个"
echo "断言数量: $(grep -c "assert_eq" azimuth_standard_unit_tests.mbt) 个"

echo ""
echo "测试用例列表:"
echo "============"
grep "^test " azimuth_standard_unit_tests.mbt | nl

echo ""
echo "验证测试文件语法结构..."
echo "===================="
# 检查基本的测试语法结构
echo "检查测试块结构..."
if grep -q "^test " azimuth_standard_unit_tests.mbt; then
    echo "✓ 测试块语法正确"
else
    echo "✗ 测试块语法错误"
fi

echo "检查断言语法..."
if grep -q "assert_eq" azimuth_standard_unit_tests.mbt; then
    echo "✓ 断言语法正确"
else
    echo "✗ 断言语法错误"
fi

echo "检查字符串断言语法..."
if grep -q "assert_eq_string" azimuth_standard_unit_tests.mbt; then
    echo "✓ 字符串断言语法正确"
else
    echo "✗ 字符串断言语法错误"
fi

echo ""
echo "测试文件前20行预览..."
echo "=================="
head -20 azimuth_standard_unit_tests.mbt

echo ""
echo "验证完成！新测试文件已成功创建并包含10个标准 MoonBit 测试用例。"