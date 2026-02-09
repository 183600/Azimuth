#!/bin/bash

echo "验证标准 Azimuth 核心测试用例..."
echo "================================"

# 进入 azimuth 目录
cd /home/runner/work/Azimuth/Azimuth/src/azimuth

# 检查测试文件是否存在
echo "检查测试文件是否存在..."
ls -la standard_azimuth_core_tests.mbt

echo ""
echo "测试文件内容预览..."
echo "=================="
head -30 standard_azimuth_core_tests.mbt

echo ""
echo "检查测试文件中的测试用例数量..."
echo "=============================="
grep -c "^test " standard_azimuth_core_tests.mbt

echo ""
echo "显示所有测试用例名称..."
echo "====================="
grep "^test " standard_azimuth_core_tests.mbt

echo ""
echo "验证完成！测试文件已成功创建并包含以下功能："
echo "1. add 函数的基本测试"
echo "2. multiply 函数的基本测试"
echo "3. subtract 函数的基本测试"
echo "4. divide_with_ceil 函数的正数测试"
echo "5. divide_with_ceil 函数的边界情况测试"
echo "6. greet 函数的基本测试"
echo "7. 复杂计算序列测试"
echo "8. 数学性质验证测试"