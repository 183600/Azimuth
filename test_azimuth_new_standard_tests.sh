#!/bin/bash

# 测试新创建的 azimuth_new_standard_tests.mbt
echo "Testing azimuth_new_standard_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"

# 测试 azimuth_new_standard_tests.mbt
echo "Testing azimuth_new_standard_tests.mbt..."
cd "$PROJECT_ROOT"

# 编译测试文件
node "$PROJECT_ROOT/moonc.js" check -pkg new_standard_test -std-path "$CORE_PATH" azimuth_new_standard_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_new_standard_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "azimuth_new_standard_tests.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in azimuth_new_standard_tests.mbt"
echo "All tests compiled successfully!"

# 显示测试内容摘要
echo ""
echo "Test Summary:"
echo "- array_operations: Array operations and index access"
echo "- boolean_logic: Boolean operations and logical expressions"
echo "- conditional_expressions: Conditional statements and ternary operators"
echo "- string_manipulation: String concatenation and comparison"
echo "- numeric_conversions: Number operations and type conversions"
echo "- error_handling: Error handling and safety checks"
echo "- loop_simulation: Recursive functions as loop alternatives"
echo "- data_structures: Tuple operations and data structure usage"
echo "- functional_operations: Higher-order functions and function composition"
echo "- edge_cases: Boundary values and special cases"

echo ""
echo "All $TEST_COUNT standard MoonBit tests are ready!"