#!/bin/bash

# 运行质量测试套件的脚本

echo "运行 Azimuth 质量测试套件..."
echo "================================"

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"

# 编译测试文件
echo "编译测试文件..."
cd "$AZIMUTH_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -include-doctests quality_test_suite.mbt

if [ $? -eq 0 ]; then
  echo "✅ 测试文件编译成功！"
  echo ""
  echo "📋 测试用例列表："
  echo "1. basic_arithmetic_operations - 基本算术运算"
  echo "2. zero_and_edge_cases - 零值和边界情况"
  echo "3. negative_number_operations - 负数运算"
  echo "4. string_greeting_functionality - 字符串问候功能"
  echo "5. complex_calculation_scenario - 复合计算场景"
  echo "6. division_ceil_behavior - 向上取整除法行为"
  echo "7. large_number_operations - 大数运算"
  echo "8. practical_business_calculation - 实际业务计算"
  echo "9. edge_case_division_by_one - 除数为1的边界情况"
  echo "10. calculation_chain_validation - 计算链验证"
  echo ""
  echo "🎯 测试覆盖范围："
  echo "- 核心算术函数：add, multiply, subtract, divide_with_ceil"
  echo "- 字符串函数：greet"
  echo "- 边界情况和错误处理"
  echo "- 实际业务应用场景"
  echo ""
  echo "✅ 所有测试用例已准备就绪，可以通过 MoonBit 测试框架运行"
else
  echo "❌ 测试文件编译失败"
  exit 1
fi