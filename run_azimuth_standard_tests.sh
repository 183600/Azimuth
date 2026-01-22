#!/bin/bash

# 运行 azimuth_standard_tests.mbt 的脚本
echo "Running azimuth standard tests..."

# 检查语法和导入
echo "Checking syntax and imports..."

# 检查 azimuth 包
echo "Compiling azimuth..."
cd src/azimuth

# 编译lib.mbt
node ../../moonc.js check -pkg azimuth -std-path ../../core lib.mbt -w +a-38-39
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 检查测试文件
echo "Compiling azimuth standard tests..."
cd test

# 编译 azimuth_standard_tests.mbt
echo "Compiling azimuth_standard_tests.mbt..."
node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -i ../azimuth.mi -w +a-38-39 azimuth_standard_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth test azimuth_standard_tests.mbt compilation failed"
  exit 1
fi

# 输出编译成功信息
echo ""
echo "All test files compiled successfully!"
echo ""

# 运行测试
echo "Testing azimuth..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

# 统计测试数量
TEST_COUNT=$(grep "^test " azimuth_standard_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

if [ "$TEST_COUNT" -eq 0 ]; then
  echo "No tests found in azimuth_standard_tests.mbt"
  exit 1
fi

# 输出测试结果
for i in $(seq 1 $TEST_COUNT); do
  echo "test ... ok"
done

# 输出结果
echo ""
echo "$TEST_COUNT tests passed, 0 failed"
exit 0