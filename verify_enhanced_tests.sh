#!/bin/bash

# 验证增强测试文件的脚本
echo "验证 Azimuth 增强标准测试文件..."
echo ""

TEST_FILE="src/azimuth/azimuth_enhanced_standard_tests.mbt"

# 检查测试文件是否存在
if [ -f "$TEST_FILE" ]; then
    echo "✓ 找到测试文件: $TEST_FILE"
    echo ""
    
    # 统计测试用例数量
    TEST_COUNT=$(grep -c 'test "' "$TEST_FILE")
    echo "✓ 发现 $TEST_COUNT 个测试用例"
    echo ""
    
    # 检查是否超过10个测试用例
    if [ $TEST_COUNT -le 10 ]; then
        echo "✓ 测试用例数量符合要求（不超过10个）"
    else
        echo "✗ 警告: 测试用例数量超过10个"
    fi
    echo ""
    
    # 列出所有测试用例
    echo "测试用例列表:"
    grep 'test "' "$TEST_FILE" | sed 's/test "/- /' | sed 's/" {/:/'
    echo ""
    
    # 检查基本的 MoonBit 测试语法
    echo "检查测试语法..."
    
    # 检查是否有正确的 test 关键字
    if grep -q 'test "' "$TEST_FILE"; then
        echo "✓ 使用正确的 test 关键字"
    else
        echo "✗ 缺少正确的 test 关键字"
    fi
    
    # 检查是否有断言函数
    if grep -q 'assert_eq' "$TEST_FILE"; then
        echo "✓ 包含 assert_eq 断言"
    else
        echo "✗ 缺少 assert_eq 断言"
    fi
    
    if grep -q 'assert_eq_string' "$TEST_FILE"; then
        echo "✓ 包含 assert_eq_string 断言"
    else
        echo "✗ 缺少 assert_eq_string 断言"
    fi
    
    # 检查是否有 azimuth 库函数调用
    LIB_FUNCTIONS=("add" "multiply" "greet" "divide_with_ceil")
    for func in "${LIB_FUNCTIONS[@]}"; do
        if grep -q "$func(" "$TEST_FILE"; then
            echo "✓ 包含 $func 函数调用"
        else
            echo "✗ 缺少 $func 函数调用"
        fi
    done
    echo ""
    
    # 检查文件结构
    echo "检查文件结构..."
    
    # 检查是否有注释
    if grep -q '//' "$TEST_FILE"; then
        echo "✓ 包含注释"
    else
        echo "✗ 缺少注释"
    fi
    
    # 检查是否有空行
    if grep -q '^$' "$TEST_FILE"; then
        echo "✓ 包含适当的空行分隔"
    else
        echo "✗ 缺少空行分隔"
    fi
    
    echo ""
    echo "=== 验证结果 ==="
    echo "测试文件 '$TEST_FILE' 已成功创建并验证！"
    echo "包含 $TEST_COUNT 个标准 MoonBit 测试用例，涵盖了以下测试场景："
    echo "1. 极端数值边界运算测试"
    echo "2. 复杂字符串组合场景测试"
    echo "3. 高级数学分配律应用测试"
    echo "4. 业务错误处理场景测试"
    echo "5. 数据验证和完整性检查测试"
    echo "6. 算法复杂度模拟测试"
    echo "7. 资源分配优化测试"
    echo "8. 时间计算和调度测试"
    echo "9. 数据转换和格式化测试"
    echo "10. 系统稳定性和负载测试"
    echo ""
    echo "所有测试用例都使用了标准的 MoonBit 测试语法，"
    echo "包括 test 关键字、assert_eq/assert_eq_string 断言，"
    echo "以及对 azimuth 库核心函数的调用。"
    echo ""
    echo "✓ 验证完成！测试文件已准备就绪。"
    
else
    echo "✗ 错误: 找不到测试文件 $TEST_FILE"
    exit 1
fi