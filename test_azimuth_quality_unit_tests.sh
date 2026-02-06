#!/bin/bash

# 测试新创建的 azimuth_quality_unit_tests.mbt
echo "Testing azimuth_quality_unit_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 测试 azimuth_quality_unit_tests.mbt
echo "Testing azimuth_quality_unit_tests.mbt..."
cd "$AZIMUTH_PATH"

# 编译测试文件
node "$AZIMUTH_PATH/moonc.js" check -pkg azimuth azimuth_quality_unit_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_quality_unit_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "azimuth_quality_unit_tests.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in azimuth_quality_unit_tests.mbt"
echo "All tests compiled successfully!"

# 显示测试内容摘要
echo ""
echo "Test Summary:"
echo "- add_function_comprehensive: 加法函数综合测试"
echo "- multiply_function_edge_cases: 乘法函数边界情况测试"
echo "- divide_with_ceil_comprehensive: 向上取整除法综合测试"
echo "- divide_with_ceil_negative_numbers: 向上取整除法负数测试"
echo "- greet_function_various_inputs: 问候函数各种输入测试"
echo "- subtract_function_comprehensive: 减法函数综合测试"
echo "- mathematical_properties_validation: 数学性质验证测试"
echo "- business_packaging_scenario: 业务包装场景测试"
echo "- complex_workflow_calculation: 复杂工作流计算测试"
echo "- edge_cases_and_boundary_values: 边界情况和极值测试"

echo ""
echo "All $TEST_COUNT standard MoonBit tests are ready!"