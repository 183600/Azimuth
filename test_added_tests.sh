#!/bin/bash

# 测试在 lib.mbt 中添加的新测试用例
echo "Testing new test cases added to lib.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

cd "$AZIMUTH_PATH"

# 1. 编译主包
echo "Compiling azimuth package with new test cases..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth" -std-path "$PROJECT_ROOT/core" -o "azimuth.mi" lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth package compilation failed"
  exit 1
fi

# 2. 统计新添加的测试用例数量
echo ""
echo "New test cases added to lib.mbt:"
grep "^test \"add_function_boundary_values\"" lib.mbt -A 100 | grep "^test " | head -10 | sed 's/test "/- /' | sed 's/" {/:/'

echo ""
echo "All tests compiled successfully!"
echo "The new test cases include the following test categories:"
echo "1. Boundary value tests for add function"
echo "2. Boundary value tests for multiply function"
echo "3. Special cases for divide_with_ceil function"
echo "4. Unicode character handling for greet function"
echo "5. Complex arithmetic combinations"
echo "6. Comprehensive negative number operations"
echo "7. Mathematical properties verification"
echo "8. Real-world financial calculations"
echo "9. Error handling and edge cases"
echo "10. Resource allocation calculations"
echo "11. Time and scheduling calculations"

exit 0