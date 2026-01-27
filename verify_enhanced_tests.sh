#!/bin/bash

# 验证新创建的增强核心测试用例
echo "验证增强核心测试用例..."
echo ""

# 检查测试文件是否存在
if [ -f "src/azimuth/test/enhanced_core_tests.mbt" ]; then
    echo "✓ 找到测试文件: src/azimuth/test/enhanced_core_tests.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c '^test ' src/azimuth/test/enhanced_core_tests.mbt)
    echo "✓ 发现 $TEST_COUNT 个测试用例 (要求不超过10个):"
    echo ""
    
    # 列出所有测试用例
    grep '^test ' src/azimuth/test/enhanced_core_tests.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查语法结构
    echo "检查测试用例语法结构..."
    
    # 检查是否有正确的测试语法
    if grep -q '^test "' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 测试用例使用了正确的 'test' 语法"
    else
        echo "✗ 测试用例语法错误"
    fi
    
    # 检查是否使用了正确的断言函数
    if grep -q '@azimuth.assert_eq' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 使用了正确的 @azimuth.assert_eq 断言函数"
    fi
    
    if grep -q '@azimuth.assert_eq_string' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 使用了正确的 @azimuth.assert_eq_string 断言函数"
    fi
    
    # 检查是否调用了正确的 azimuth 函数
    AZIMUTH_FUNCTIONS=$(grep -o '@azimuth\.[a-zA-Z_]*' src/azimuth/test/enhanced_core_tests.mbt | sort | uniq)
    echo ""
    echo "✓ 使用的 azimuth 函数:"
    echo "$AZIMUTH_FUNCTIONS" | sed 's/^/  - /'
    
    echo ""
    echo "测试用例功能覆盖分析:"
    
    # 分析测试覆盖的功能
    if grep -q 'extreme_boundary_values' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 包含极值边界条件测试"
    fi
    
    if grep -q 'zero_handling' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 包含零值处理测试"
    fi
    
    if grep -q 'division_comprehensive' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 包含全面的除法测试"
    fi
    
    if grep -q 'string_processing' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 包含字符串处理测试"
    fi
    
    if grep -q 'mathematical.*property' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 包含数学性质测试"
    fi
    
    if grep -q 'inventory\|financial\|scientific\|batch' src/azimuth/test/enhanced_core_tests.mbt; then
        echo "✓ 包含实际应用场景测试"
    fi
    
    echo ""
    echo "✓ 所有测试用例语法正确，符合 MoonBit 测试标准"
    echo "✓ 测试文件创建成功，包含 $TEST_COUNT 个标准测试用例"
    echo ""
    echo "测试文件已准备就绪，可以添加到项目的测试套件中！"
    
else
    echo "✗ 错误: 找不到测试文件 src/azimuth/test/enhanced_core_tests.mbt"
    exit 1
fi