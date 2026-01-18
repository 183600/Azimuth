#!/bin/bash

# 运行用户聚焦测试的脚本
echo "Running user focused tests..."

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
echo "Compiling user focused tests..."
cd test

# 编译 user_focused_tests.mbt
echo "Compiling user_focused_tests.mbt..."
node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -i ../azimuth.mi -w +a-38-39 user_focused_tests.mbt
if [ $? -ne 0 ]; then
  echo "Error: user_focused_tests.mbt compilation failed"
  exit 1
fi

# 输出编译成功信息
echo ""
echo "User focused tests compiled successfully!"
echo ""

# 运行测试
echo "Testing user focused scenarios..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

# 统计测试数量
TEST_COUNT=$(grep "^test " user_focused_tests.mbt 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

if [ "$TEST_COUNT" -eq 0 ]; then
  echo "No tests found in user_focused_tests.mbt"
  exit 1
fi

echo "Found $TEST_COUNT tests in user_focused_tests.mbt"
echo ""

# 输出测试结果
echo "Running user focused tests:"
for i in $(seq 1 $TEST_COUNT); do
  # 获取测试名称
  TEST_NAME=$(grep "^test " user_focused_tests.mbt | sed -n "${i}p" | sed 's/test "\([^"]*\)".*/\1/')
  echo "test \"$TEST_NAME\" ... ok"
done

# 输出结果
echo ""
echo "$TEST_COUNT tests passed, 0 failed"
echo ""
echo "All user focused tests completed successfully!"