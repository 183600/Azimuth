#!/bin/bash

# 测试新创建的高级测试用例
echo "Testing new advanced moonbit test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 测试新创建的高级测试用例
echo ""
echo "Testing new advanced test cases..."
cd "$AZIMUTH_PATH/test"

# 编译新测试文件
echo "Checking advanced_moonbit_test_cases.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi advanced_moonbit_test_cases.mbt
if [ $? -ne 0 ]; then
  echo "Error: advanced_moonbit_test_cases.mbt compilation failed"
  exit 1
else
  echo "advanced_moonbit_test_cases.mbt compiled successfully!"
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "advanced_moonbit_test_cases.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo ""
echo "Found $TEST_COUNT test cases in advanced_moonbit_test_cases.mbt"
echo "All tests syntax checked successfully!"
echo ""
echo "Test cases included:"
grep "^test " "advanced_moonbit_test_cases.mbt" | sed 's/test "/- /' | sed 's/" {/:/'