#!/bin/bash

echo "分析测试失败原因..."
echo ""

# 检查源代码中的潜在问题
echo "1. 检查lib.mbt文件中的函数实现..."
echo "检查add函数..."
grep -A 50 "pub fn add" src/clean_test/lib.mbt | head -30

echo ""
echo "检查multiply函数..."
grep -A 50 "pub fn multiply" src/clean_test/lib.mbt | head -30

echo ""
echo "检查greet函数..."
grep -A 10 "pub fn greet" src/clean_test/lib.mbt

echo ""
echo "2. 检查测试用例中可能存在的问题..."
echo "查找所有测试用例..."
grep -n 'test "' src/clean_test/test/lib_test.mbt | head -10

echo ""
echo "3. 检查是否有编译错误..."
echo "模拟编译检查..."

# 检查语法错误
echo "检查语法错误..."
if node -c moonc.js 2>/dev/null; then
    echo "moonc.js语法正确"
else
    echo "moonc.js有语法错误"
fi

echo ""
echo "4. 分析可能的问题点..."
echo "a) 检查测试用例中的断言是否有问题"
echo "b) 检查函数实现是否与测试期望一致"
echo "c) 检查是否有类型错误"

echo ""
echo "分析完成。"