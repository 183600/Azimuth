#!/bin/bash

# 验证新创建的测试文件

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

echo "验证新创建的测试文件..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "编译 azimuth 包..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi
if [ $? -ne 0 ]; then
  echo "错误：azimuth/lib.mbt 编译失败"
  exit 1
fi

# 编译新的测试文件（与 lib.mbt 一起编译）
echo "编译新的测试文件..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt standard_moonbit_test_cases_new.mbt
if [ $? -ne 0 ]; then
  echo "错误：测试文件编译失败"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " standard_moonbit_test_cases_new.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo ""
echo "测试验证成功！"
echo "测试文件：standard_moonbit_test_cases_new.mbt"
echo "测试用例数量：$TEST_COUNT"
echo ""
echo "测试用例列表："
grep "^test " standard_moonbit_test_cases_new.mbt

exit 0