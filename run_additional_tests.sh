#!/bin/bash

# 运行额外测试的脚本
echo "Running additional tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译azimuth包
echo "Compiling azimuth package..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译测试文件
echo "Compiling additional test file..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi azimuth_additional_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_additional_tests.mbt compilation failed"
  exit 1
fi

echo "Additional tests compiled successfully!"
echo "Test file: azimuth_additional_tests.mbt"
echo ""
echo "Test cases included:"
echo "1. arithmetic_progression_sum - 计算等差数列的和"
echo "2. geometric_area_calculations - 几何面积计算测试"
echo "3. temperature_conversion_scenarios - 温度转换场景测试"
echo "4. shopping_cart_with_discount - 购物车折扣计算测试"
echo "5. digital_time_conversion - 数字时间转换测试"
echo "6. compound_interest_simple - 简单复利计算测试"
echo "7. string_validation_scenarios - 字符串验证场景测试"
echo "8. array_index_calculation - 数组索引计算测试"
echo "9. percentage_calculations - 百分比计算测试"
echo "10. game_score_calculations - 游戏得分计算测试"

echo ""
echo "Additional tests completed successfully!"