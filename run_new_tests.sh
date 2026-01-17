#!/bin/bash

# 运行新创建的测试文件

echo "运行新创建的标准测试文件..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 编译 azimuth 包
echo "编译 azimuth 包..."
cd "$AZIMUTH_PATH"

# 使用 moonc.js 编译 lib.mbt
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "错误: azimuth/lib.mbt 编译失败"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi
if [ $? -ne 0 ]; then
  echo "警告: 生成 azimuth.mi 文件失败"
fi

# 编译新的测试文件
echo "编译新的测试文件..."
cd test

# 测试 azimuth_new_standard_tests.mbt
echo "检查 azimuth_new_standard_tests.mbt..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" test/azimuth_new_standard_tests.mbt
if [ $? -ne 0 ]; then
  echo "错误: azimuth_new_standard_tests.mbt 有编译问题"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " test/azimuth_new_standard_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo ""
echo "=== 测试结果 ==="
echo "测试文件: azimuth_new_standard_tests.mbt"
echo "测试数量: $TEST_COUNT"
echo "编译状态: 成功"
echo ""
echo "测试列表:"
grep "^test " test/azimuth_new_standard_tests.mbt

echo ""
echo "所有 $TEST_COUNT 个测试编译成功！"