#!/bin/bash

# 测试新添加的标准 MoonBit 测试用例
echo "Testing additional_standard_moonbit_tests.mbt file..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

cd "$AZIMUTH_PATH"

# 1. 编译主包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth" -std-path "$PROJECT_ROOT/core" -o "azimuth.mi" lib.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: azimuth package compilation failed"
  exit 1
fi

# 2. 编译新添加的测试文件
echo "Compiling additional_standard_moonbit_tests.mbt file..."
cd "$PROJECT_ROOT/test"

node "$PROJECT_ROOT/moonc.js" check -pkg "azimuth_test" -std-path "$PROJECT_ROOT/core" -i "../src/azimuth/azimuth.mi" additional_standard_moonbit_tests.mbt
if [ $? -ne 0 ]; then
  echo "ERROR: additional_standard_moonbit_tests.mbt file compilation failed"
  exit 1
fi

# 3. 统计测试数量
TEST_COUNT=$(grep "^test " additional_standard_moonbit_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in additional_standard_moonbit_tests.mbt"

# 4. 列出所有测试用例
echo ""
echo "Test cases found:"
grep "^test " additional_standard_moonbit_tests.mbt | sed 's/test "/- /' | sed 's/" {/:/'

echo ""
echo "All tests compiled successfully!"
echo "The new test file includes the following test categories:"
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