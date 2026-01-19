#!/bin/bash

# 运行新添加的 MoonBit 测试用例的脚本
echo "Running newly added MoonBit test cases..."

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

# 编译新添加的测试文件
echo "Compiling newly added test file..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi additional_moonbit_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: additional_moonbit_tests.mbt compilation failed"
  exit 1
fi

echo "Newly added test cases compiled successfully!"
echo "Test file: test/additional_moonbit_tests.mbt"
echo ""
echo "Test cases included:"
echo "1. add_commutative_property - 测试加法交换律"
echo "2. multiply_associative_property - 测试乘法结合律"
echo "3. distributive_property - 测试分配律"
echo "4. temperature_conversion_celsius_to_fahrenheit - 温度转换测试"
echo "5. simple_interest_calculation - 简单利息计算测试"
echo "6. area_perimeter_calculations - 面积和周长计算测试"
echo "7. string_edge_cases - 字符串边界情况测试"
echo "8. extreme_value_operations - 极值运算测试"
echo "9. sequential_operations - 连续运算测试"

echo ""
echo "Newly added test cases completed successfully!"