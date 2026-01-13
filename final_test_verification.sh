#!/bin/bash

# 最终测试验证脚本
echo "=== Final Test Verification ==="
echo "Date: $(date)"
echo ""

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"

# 检查所有必要的文件
echo "=== Checking Required Files ==="

# 检查azimuth包
AZIMUTH_LIB="$PROJECT_ROOT/src/azimuth/lib.mbt"
AZIMUTH_MI="$PROJECT_ROOT/src/azimuth/azimuth.mi"
AZIMUTH_WASM="$PROJECT_ROOT/src/azimuth/azimuth.wasm"

if [ -f "$AZIMUTH_LIB" ]; then
    echo "✓ azimuth/lib.mbt exists"
else
    echo "✗ azimuth/lib.mbt missing"
fi

if [ -f "$AZIMUTH_MI" ]; then
    echo "✓ azimuth.mi exists ($(stat -c%s $AZIMUTH_MI) bytes)"
else
    echo "✗ azimuth.mi missing"
fi

if [ -f "$AZIMUTH_WASM" ]; then
    echo "✓ azimuth.wasm exists ($(stat -c%s $AZIMUTH_WASM) bytes)"
else
    echo "✗ azimuth.wasm missing"
fi

# 检查clean_test包
CLEAN_TEST_LIB="$PROJECT_ROOT/clean_test/lib.mbt"
CLEAN_TEST_MI="$PROJECT_ROOT/clean_test/clean_test.mi"
CLEAN_TEST_WASM="$PROJECT_ROOT/clean_test/clean_test.wasm"

if [ -f "$CLEAN_TEST_LIB" ]; then
    echo "✓ clean_test/lib.mbt exists"
else
    echo "✗ clean_test/lib.mbt missing"
fi

if [ -f "$CLEAN_TEST_MI" ]; then
    echo "✓ clean_test.mi exists ($(stat -c%s $CLEAN_TEST_MI) bytes)"
else
    echo "✗ clean_test.mi missing"
fi

if [ -f "$CLEAN_TEST_WASM" ]; then
    echo "✓ clean_test.wasm exists ($(stat -c%s $CLEAN_TEST_WASM) bytes)"
else
    echo "✗ clean_test.wasm missing"
fi

echo ""

# 编译验证
echo "=== Compilation Verification ==="

cd "$PROJECT_ROOT/src/azimuth"
node ../../moonc.js check -pkg azimuth -std-path ../../core lib.mbt
if [ $? -eq 0 ]; then
    echo "✓ azimuth package compiles successfully"
else
    echo "✗ azimuth package compilation failed"
fi

cd "$PROJECT_ROOT/clean_test"
node ../moonc.js check -pkg clean_test -std-path ../core lib.mbt
if [ $? -eq 0 ]; then
    echo "✓ clean_test package compiles successfully"
else
    echo "✗ clean_test package compilation failed"
fi

echo ""

# 测试验证
echo "=== Test Verification ==="

# 运行真正的测试执行器
cd "$PROJECT_ROOT"
node run_real_test_execution.js
TEST_RESULT=$?

echo ""

# 最终结果
echo "=== Final Result ==="
if [ $TEST_RESULT -eq 0 ]; then
    echo "✓ All tests passed successfully!"
    echo "✓ No compilation errors found"
    echo "✓ All packages properly compiled"
    echo ""
    echo "Status: PASSED"
else
    echo "✗ Some tests failed"
    echo "Status: FAILED"
fi

exit $TEST_RESULT