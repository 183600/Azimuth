#!/bin/bash

# 运行新创建的核心测试用例的脚本
echo "Running new standard moonbit core tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译 azimuth 包
echo "Compiling azimuth package..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 编译新的核心测试文件
echo "Compiling new core tests..."
cd "$PROJECT_ROOT/test"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi standard_moonbit_core_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: standard_moonbit_core_tests.mbt compilation failed"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " standard_moonbit_core_tests.mbt | wc -l)
echo ""
echo "Successfully compiled $TEST_COUNT test cases in standard_moonbit_core_tests.mbt"
echo ""

# 提取测试名称
echo "Test cases found:"
grep "^test " standard_moonbit_core_tests.mbt | sed 's/test "\(.*\)" {/- \1/'
echo ""

echo "All new core tests compiled successfully!"