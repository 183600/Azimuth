#!/bin/bash

echo "运行高质量标准测试用例..."
echo "============================"

# 进入 azimuth 目录
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查测试文件是否存在
echo "检查测试文件是否存在..."
ls -la high_quality_standard_tests.mbt

echo ""
echo "尝试检查语法..."
# 尝试使用 moonc 检查语法
if command -v moonc &> /dev/null; then
    echo "使用 moonc 检查语法..."
    moonc check high_quality_standard_tests.mbt 2>&1 | head -20
else
    echo "moonc 命令不可用，跳过语法检查"
fi

echo ""
echo "尝试运行测试..."
# 尝试使用不同的方式运行测试
if command -v moon &> /dev/null; then
    echo "使用 moon 命令运行测试..."
    moon test high_quality_standard_tests.mbt 2>&1 | head -50
elif command -v moonc &> /dev/null; then
    echo "尝试使用 moonc 运行测试..."
    moonc test high_quality_standard_tests.mbt 2>&1 | head -50
else
    echo "moon 和 moonc 命令都不可用，无法运行测试"
    echo "但测试文件已成功创建并添加到项目中"
fi

echo ""
echo "测试文件内容预览..."
echo "=================="
head -50 high_quality_standard_tests.mbt