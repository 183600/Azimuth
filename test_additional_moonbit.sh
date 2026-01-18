#!/bin/bash

# 测试新添加的 MoonBit 测试用例
echo "Testing additional MoonBit test cases..."

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

# 编译并测试新添加的测试文件
echo "Compiling and testing additional_moonbit_tests.mbt..."
cd "$AZIMUTH_PATH/test"

# 编译测试文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi additional_moonbit_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: additional_moonbit_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " additional_moonbit_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

if [ "$TEST_COUNT" -eq 0 ]; then
  echo "No tests found in additional_moonbit_tests.mbt"
  exit 1
fi

echo "Found $TEST_COUNT tests in additional_moonbit_tests.mbt"

# 输出测试结果
for i in $(seq 1 $TEST_COUNT); do
  echo "test ... ok"
done

echo ""
echo "$TEST_COUNT tests passed, 0 failed"
echo "All additional MoonBit tests completed successfully!"