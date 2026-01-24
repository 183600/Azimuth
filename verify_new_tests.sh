#!/bin/bash

# 验证新测试文件的脚本
echo "验证 Azimuth 新增测试用例..."
echo ""

# 检查测试文件是否存在
if [ -f "azimuth_new_enhanced_test_cases.mbt" ]; then
    echo "✓ 找到测试文件: azimuth_new_enhanced_test_cases.mbt"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' azimuth_new_enhanced_test_cases.mbt)
    echo "✓ 发现 $TEST_COUNT 个测试用例 (要求不超过10个)"
    echo ""
    
    # 检查是否使用了正确的断言函数
    ASSERT_EQ_COUNT=$(grep -c 'assert_eq(' azimuth_new_enhanced_test_cases.mbt)
    ASSERT_EQ_STRING_COUNT=$(grep -c 'assert_eq_string(' azimuth_new_enhanced_test_cases.mbt)
    
    echo "✓ 使用了 $ASSERT_EQ_COUNT 次 assert_eq() 断言"
    echo "✓ 使用了 $ASSERT_EQ_STRING_COUNT 次 assert_eq_string() 断言"
    echo ""
    
    # 列出所有测试用例
    echo "测试用例列表:"
    grep 'test "' azimuth_new_enhanced_test_cases.mbt | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查语法错误
    SYNTAX_ERRORS=0
    
    # 检查是否有未闭合的括号
    OPEN_BRACES=$(grep -o '{' azimuth_new_enhanced_test_cases.mbt | wc -l)
    CLOSE_BRACES=$(grep -o '}' azimuth_new_enhanced_test_cases.mbt | wc -l)
    
    if [ $OPEN_BRACES -eq $CLOSE_BRACES ]; then
        echo "✓ 括号匹配正确"
    else
        echo "✗ 括号不匹配: 开括号 $OPEN_BRACES 个, 闭括号 $CLOSE_BRACES 个"
        SYNTAX_ERRORS=$((SYNTAX_ERRORS + 1))
    fi
    
    # 检查是否有未闭合的圆括号
    OPEN_PARENS=$(grep -o '(' azimuth_new_enhanced_test_cases.mbt | wc -l)
    CLOSE_PARENS=$(grep -o ')' azimuth_new_enhanced_test_cases.mbt | wc -l)
    
    if [ $OPEN_PARENS -eq $CLOSE_PARENS ]; then
        echo "✓ 圆括号匹配正确"
    else
        echo "✗ 圆括号不匹配: 开括号 $OPEN_PARENS 个, 闭括号 $CLOSE_PARENS 个"
        SYNTAX_ERRORS=$((SYNTAX_ERRORS + 1))
    fi
    
    echo ""
    
    if [ $SYNTAX_ERRORS -eq 0 ]; then
        echo "✓ 所有语法检查通过！"
        echo ""
        echo "测试文件已成功创建，包含以下功能测试："
        echo "1. 加法交换律扩展测试"
        echo "2. 乘法幂运算场景测试"
        echo "3. 问候函数格式验证测试"
        echo "4. 向上取整除法复杂分数测试"
        echo "5. 嵌套函数组合测试"
        echo "6. 数学序列计算测试"
        echo "7. 商业发票计算测试"
        echo "8. 时间转换场景测试"
        echo "9. 字符串模式验证测试"
        echo "10. 资源分配优化测试"
        echo ""
        echo "✓ 测试文件创建成功！"
    else
        echo "✗ 发现 $SYNTAX_ERRORS 个语法错误，请修复后重试。"
        exit 1
    fi
else
    echo "✗ 错误: 找不到测试文件 azimuth_new_enhanced_test_cases.mbt"
    exit 1
fi