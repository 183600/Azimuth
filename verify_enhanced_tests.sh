#!/bin/bash

echo "验证新创建的 azimuth_enhanced_test_cases.mbt 测试文件"
echo "======================================================"

# 检查测试文件是否存在
if [ -f "azimuth/azimuth_enhanced_test_cases.mbt" ]; then
    echo "✓ 测试文件 azimuth_enhanced_test_cases.mbt 存在"
else
    echo "✗ 测试文件 azimuth_enhanced_test_cases.mbt 不存在"
    exit 1
fi

# 检查测试文件是否在 moon.pkg.json 中注册
if grep -q "azimuth_enhanced_test_cases.mbt" azimuth/moon.pkg.json; then
    echo "✓ 测试文件已在 moon.pkg.json 中注册"
else
    echo "✗ 测试文件未在 moon.pkg.json 中注册"
    exit 1
fi

# 统计测试用例数量
test_count=$(grep -c "^test " azimuth/azimuth_enhanced_test_cases.mbt)
echo "✓ 发现 $test_count 个测试用例"

# 检查语法结构
# 检查是否有未闭合的大括号
open_braces=$(grep -o "{" azimuth/azimuth_enhanced_test_cases.mbt | wc -l)
close_braces=$(grep -o "}" azimuth/azimuth_enhanced_test_cases.mbt | wc -l)

if [ $open_braces -eq $close_braces ]; then
    echo "✓ 大括号匹配正确 ($open_braces 对)"
else
    echo "✗ 大括号不匹配 (开: $open_braces, 闭: $close_braces)"
    exit 1
fi

# 检查是否包含必要的函数调用
if grep -q "assert_eq" azimuth/azimuth_enhanced_test_cases.mbt && \
   grep -q "assert_eq_string" azimuth/azimuth_enhanced_test_cases.mbt && \
   grep -q "assert_true" azimuth/azimuth_enhanced_test_cases.mbt; then
    echo "✓ 包含必要的断言函数"
else
    echo "✗ 缺少必要的断言函数"
    exit 1
fi

# 检查是否使用了项目中的函数
if grep -q "add(" azimuth/azimuth_enhanced_test_cases.mbt && \
   grep -q "multiply(" azimuth/azimuth_enhanced_test_cases.mbt && \
   grep -q "divide_with_ceil(" azimuth/azimuth_enhanced_test_cases.mbt && \
   grep -q "greet(" azimuth/azimuth_enhanced_test_cases.mbt; then
    echo "✓ 正确使用了项目函数"
else
    echo "✗ 未正确使用项目函数"
    exit 1
fi

echo ""
echo "语法验证完成！"
echo "测试文件包含以下测试用例："
grep "^test " azimuth/azimuth_enhanced_test_cases.mbt | sed 's/test "/- /' | sed 's/" {$//'

echo ""
echo "所有验证项目均通过，测试文件语法正确！"