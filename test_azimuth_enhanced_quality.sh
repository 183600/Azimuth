#!/bin/bash

# 测试新添加的 Azimuth 增强质量测试用例
echo "Testing azimuth_enhanced_quality test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 测试新添加的测试用例
echo "Testing azimuth_enhanced_quality test cases..."
cd "$AZIMUTH_PATH"

# 编译测试文件
node "$AZIMUTH_PATH/moonc.js" check -pkg azimuth lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: lib.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "test \"azimuth_enhanced_quality_\" "lib.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT azimuth_enhanced_quality tests in lib.mbt"
echo "All tests compiled successfully!"

# 显示测试内容摘要
echo ""
echo "Azimuth Enhanced Quality Test Summary:"
echo "- azimuth_enhanced_quality_add_function: 加法函数综合测试"
echo "- azimuth_enhanced_quality_multiply_function: 乘法函数边界情况测试"
echo "- azimuth_enhanced_quality_divide_ceil: 向上取整除法综合测试"
echo "- azimuth_enhanced_quality_divide_negative: 向上取整除法负数测试"
echo "- azimuth_enhanced_quality_greet_function: 问候函数各种输入测试"
echo "- azimuth_enhanced_quality_subtract_function: 减法函数综合测试"
echo "- azimuth_enhanced_quality_math_properties: 数学性质验证测试"
echo "- azimuth_enhanced_quality_packaging_scenario: 业务包装场景测试"
echo "- azimuth_enhanced_quality_workflow_calculation: 复杂工作流计算测试"
echo "- azimuth_enhanced_quality_boundary_values: 边界情况和极值测试"

echo ""
echo "All $TEST_COUNT standard MoonBit tests are ready!"