#!/bin/bash

# 测试新创建的 azimuth_new_tests.mbt
echo "Testing azimuth_new_tests.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"

# 测试 azimuth_new_tests.mbt
echo "Testing azimuth_new_tests.mbt..."
cd "$PROJECT_ROOT"

# 编译测试文件
node "$PROJECT_ROOT/moonc.js" check -pkg new_test -std-path "$CORE_PATH" azimuth_new_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth_new_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "azimuth_new_tests.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in azimuth_new_tests.mbt"
echo "All tests compiled successfully!"

# 显示测试内容摘要
echo ""
echo "Test Summary:"
echo "- array_operations: Array manipulation and iteration"
echo "- boolean_logic: Boolean operations and conditions"
echo "- conditional_calculations: Conditional expressions"
echo "- string_manipulation: String operations and concatenation"
echo "- error_handling: Error handling and edge cases"
echo "- recursive_calculations: Recursive functions (factorial, fibonacci)"
echo "- data_validation: Input validation patterns"
echo "- unit_conversion: Unit conversion utilities"
echo "- performance_benchmarks: Performance testing patterns"
echo "- edge_cases: Boundary value testing"