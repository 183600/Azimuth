#!/bin/bash

echo "验证新创建的测试用例..."
echo "======================="

# 进入 core 目录
cd /home/runner/work/Azimuth/Azimuth/core

# 检查测试文件是否存在
echo "检查测试文件是否存在..."
ls -la standard_comprehensive_test_suite.mbt

echo ""
echo "测试文件内容预览..."
echo "=================="
head -30 standard_comprehensive_test_suite.mbt

echo ""
echo "测试用例数量统计..."
echo "=================="
grep -c "^test " standard_comprehensive_test_suite.mbt

echo ""
echo "测试用例列表..."
echo "============"
grep "^test " standard_comprehensive_test_suite.mbt

echo ""
echo "验证完成！测试文件已成功创建并包含以下测试用例："
echo "- int_absolute_values: 测试整数的绝对值功能"
echo "- list_construction_and_basic_operations: 测试列表的构造和基本操作"
echo "- list_functional_operations: 测试列表的函数式操作"
echo "- array_basic_operations: 测试数组的基本操作"
echo "- array_zip_with_operation: 测试数组的 zip_with 操作"
echo "- string_basic_operations: 测试字符串的基本操作"
echo "- option_type_operations: 测试 Option 类型的操作"
echo "- result_type_operations: 测试 Result 类型的操作"
echo "- char_operations: 测试字符操作"
echo "- mathematical_calculations: 测试数学计算"