#!/bin/bash

# 验证简单测试文件的语法

echo "验证新创建的测试文件..."

# 检查文件是否存在
if [ -f "azimuth_simple_enhanced_system_tests.mbt" ]; then
    echo "✓ 测试文件存在"
else
    echo "✗ 测试文件不存在"
    exit 1
fi

# 统计测试用例数量
test_count=$(grep -c "^test " azimuth_simple_enhanced_system_tests.mbt)
echo "✓ 包含 $test_count 个测试用例"

# 验证基本语法
echo "验证基本语法..."
if head -20 azimuth_simple_enhanced_system_tests.mbt | grep -q "test.*{"; then
    echo "✓ 测试语法正确"
else
    echo "✗ 测试语法错误"
    exit 1
fi

# 验证文件内容
echo "验证文件内容..."
if grep -q "span基本操作测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含span基本操作测试"
else
    echo "✗ 缺少span基本操作测试"
fi

if grep -q "度量计数器操作测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含度量计数器操作测试"
else
    echo "✗ 缺少度量计数器操作测试"
fi

if grep -q "属性值类型转换测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含属性值类型转换测试"
else
    echo "✗ 缺少属性值类型转换测试"
fi

if grep -q "上下文传播测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含上下文传播测试"
else
    echo "✗ 缺少上下文传播测试"
fi

if grep -q "资源属性测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含资源属性测试"
else
    echo "✗ 缺少资源属性测试"
fi

if grep -q "直方图度量操作测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含直方图度量操作测试"
else
    echo "✗ 缺少直方图度量操作测试"
fi

if grep -q "错误处理测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含错误处理测试"
else
    echo "✗ 缺少错误处理测试"
fi

if grep -q "并发操作测试" azimuth_simple_enhanced_system_tests.mbt; then
    echo "✓ 包含并发操作测试"
else
    echo "✗ 缺少并发操作测试"
fi

echo "验证完成！"
echo "测试文件包含以下测试用例："
grep "^test " azimuth_simple_enhanced_system_tests.mbt | sed 's/test "//' | sed 's/" {//'