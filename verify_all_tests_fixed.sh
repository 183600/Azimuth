#!/bin/bash

# 验证所有测试都能编译通过的脚本

echo "验证所有测试编译状态..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"

# 编译 azimuth 包
echo "编译 azimuth 包..."
cd "$PROJECT_ROOT/azimuth"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" -o azimuth.mi lib.mbt
if [ $? -ne 0 ]; then
  echo "错误：azimuth 包编译失败"
  exit 1
fi

# 编译 azimuth 测试
echo "编译 azimuth 测试..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi simple_test.mbt
if [ $? -ne 0 ]; then
  echo "错误：azimuth/simple_test.mbt 编译失败"
  exit 1
fi

node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi additional_comprehensive_tests.mbt
if [ $? -ne 0 ]; then
  echo "错误：azimuth/additional_comprehensive_tests.mbt 编译失败"
  exit 1
fi

# 编译 clean_test 包
echo "编译 clean_test 包..."
cd "$PROJECT_ROOT/clean_test"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" -o clean_test.mi lib.mbt
if [ $? -ne 0 ]; then
  echo "错误：clean_test 包编译失败"
  exit 1
fi

# 编译 clean_test 测试
echo "编译 clean_test 测试..."
cd test
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi simple_test.mbt
if [ $? -ne 0 ]; then
  echo "错误：clean_test/simple_test.mbt 编译失败"
  exit 1
fi

node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi additional_comprehensive_tests.mbt
if [ $? -ne 0 ]; then
  echo "错误：clean_test/additional_comprehensive_tests.mbt 编译失败"
  exit 1
fi

# 编译 test_only 包
echo "编译 test_only 包..."
cd "$PROJECT_ROOT/test_only"
node "$PROJECT_ROOT/moonc.js" check -pkg test_only -std-path "$CORE_PATH" -o test_only.mi lib.mbt
if [ $? -ne 0 ]; then
  echo "错误：test_only 包编译失败"
  exit 1
fi

# 编译 core 测试
echo "编译 core 测试..."
cd "$PROJECT_ROOT/core/test"
node "$PROJECT_ROOT/moonc.js" check -pkg core_test -std-path "$CORE_PATH" -i ../builtin/builtin.mi test.mbt
if [ $? -ne 0 ]; then
  echo "错误：core/test.mbt 编译失败"
  exit 1
fi

node "$PROJECT_ROOT/moonc.js" check -pkg core_test -std-path "$CORE_PATH" -i ../builtin/builtin.mi test_test_simple.mbt
if [ $? -ne 0 ]; then
  echo "错误：core/test_test_simple.mbt 编译失败"
  exit 1
fi

echo ""
echo "所有测试编译成功！"
echo "18 个测试已准备就绪"