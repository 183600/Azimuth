#!/bin/bash

# 验证所有测试文件是否可以正常编译

echo "验证azimuth包的测试文件..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

AZIMUTH_ERRORS=0
AZIMUTH_TOTAL=0

for file in *.mbt; do
    if [ -f "$file" ]; then
        AZIMUTH_TOTAL=$((AZIMUTH_TOTAL + 1))
        echo -n "检查 $file ... "
        if node /home/runner/work/Azimuth/Azimuth/moonc.js check -pkg azimuth_test -std-path /home/runner/work/Azimuth/Azimuth/core -whitebox-test "$file" > /dev/null 2>&1; then
            echo "✓ 通过"
        else
            echo "✗ 失败"
            AZIMUTH_ERRORS=$((AZIMUTH_ERRORS + 1))
        fi
    fi
done

echo ""
echo "验证clean_test包的测试文件..."
cd /home/runner/work/Azimuth/Azimuth/src/clean_test/test

CLEAN_TEST_ERRORS=0
CLEAN_TEST_TOTAL=0

for file in *.mbt; do
    if [ -f "$file" ]; then
        CLEAN_TEST_TOTAL=$((CLEAN_TEST_TOTAL + 1))
        echo -n "检查 $file ... "
        if node /home/runner/work/Azimuth/Azimuth/moonc.js check -pkg clean_test_test -std-path /home/runner/work/Azimuth/Azimuth/core -whitebox-test "$file" > /dev/null 2>&1; then
            echo "✓ 通过"
        else
            echo "✗ 失败"
            CLEAN_TEST_ERRORS=$((CLEAN_TEST_ERRORS + 1))
        fi
    fi
done

echo ""
echo "======================================"
echo "验证结果:"
echo "azimuth包: $((AZIMUTH_TOTAL - AZIMUTH_ERRORS))/$AZIMUTH_TOTAL 通过"
echo "clean_test包: $((CLEAN_TEST_TOTAL - CLEAN_TEST_ERRORS))/$CLEAN_TEST_TOTAL 通过"
echo "总计: $((AZIMUTH_TOTAL + CLEAN_TEST_TOTAL - AZIMUTH_ERRORS - CLEAN_TEST_ERRORS))/$(($AZIMUTH_TOTAL + $CLEAN_TEST_TOTAL)) 通过"

if [ $AZIMUTH_ERRORS -eq 0 ] && [ $CLEAN_TEST_ERRORS -eq 0 ]; then
    echo "所有测试文件编译通过！"
    exit 0
else
    echo "有 $((AZIMUTH_ERRORS + CLEAN_TEST_ERRORS)) 个测试文件编译失败。"
    exit 1
fi