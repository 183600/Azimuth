#!/bin/bash

# 测试新创建的 azimuth_simple_standard_tests.mbt
echo "Testing azimuth_simple_standard_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 测试 azimuth_simple_standard_tests.mbt
echo "Testing azimuth_simple_standard_tests.mbt..."
cd "$AZIMUTH_PATH"

# 编译测试文件
node "$AZIMUTH_PATH/moonc.js" check -pkg azimuth azimuth_simple_standard_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_simple_standard_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "azimuth_simple_standard_tests.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in azimuth_simple_standard_tests.mbt"
echo "All tests compiled successfully!"

# 显示测试内容摘要
echo ""
echo "Test Summary:"
echo "- basic_arithmetic_operations: 基本算术运算测试"
echo "- string_operations: 字符串操作测试"
echo "- mathematical_formulas: 数学公式测试"
echo "- financial_calculations: 金融计算测试"
echo "- temperature_conversion: 温度转换测试"
echo "- array_index_calculations: 数组索引计算测试"
echo "- time_calculations: 时间计算测试"
echo "- statistical_operations: 统计运算测试"
echo "- geometry_calculations: 几何计算测试"
echo "- boundary_value_tests: 边界值测试"

echo ""
echo "All $TEST_COUNT standard MoonBit tests are ready!"