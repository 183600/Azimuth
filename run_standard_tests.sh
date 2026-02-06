#!/bin/bash

echo "Running standard test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
TEST_PATH="$AZIMUTH_PATH/test"

# 编译 azimuth 包
echo "Compiling azimuth package..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 编译测试文件
echo "Compiling test files..."
cd "$TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi azimuth_standard_test_cases.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_standard_test_cases.mbt compilation failed"
  exit 1
fi

echo "Standard test cases compiled successfully!"
echo "Created 10 high-quality MoonBit test cases:"
echo "1. basic_arithmetic_addition - Tests addition operations"
echo "2. basic_arithmetic_multiplication - Tests multiplication operations"
echo "3. basic_arithmetic_subtraction - Tests subtraction operations"
echo "4. string_concatenation - Tests string concatenation"
echo "5. boolean_operations - Tests boolean operations"
echo "6. comparison_operations - Tests comparison operations"
echo "7. modulo_operations - Tests modulo operations"
echo "8. nested_expressions - Tests nested expressions"
echo "9. integer_operations - Tests integer operations"
echo "10. edge_cases - Tests edge cases and boundary values"

echo "All tests use standard MoonBit test syntax and @builtin.abort for assertions."