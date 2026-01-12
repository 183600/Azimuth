#!/bin/bash

# 全面测试脚本 - 验证所有测试文件
echo "开始全面验证所有测试文件..."

# 生成必要的 .mi 文件
echo "生成 azimuth.mi 文件..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core -o azimuth.mi lib.mbt

echo "生成 clean_test.mi 文件..."
cd ../clean_test
node ../../moonc.js check -pkg clean_test -std-path ../../core -o clean_test.mi lib.mbt

# 测试 azimuth 包的所有测试文件
echo ""
echo "测试 azimuth 包的所有测试文件..."
cd ../azimuth/test
ERROR_COUNT=0
AZIMUTH_TEST_COUNT=0

for file in *.mbt; do
    # 跳过备份文件
    if [[ $file == *.bak* ]]; then
        continue
    fi
    
    AZIMUTH_TEST_COUNT=$((AZIMUTH_TEST_COUNT + 1))
    echo "测试文件: $file"
    if ! node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -i ../azimuth.mi "$file" 2>/dev/null; then
        echo "错误: $file 编译失败"
        ERROR_COUNT=$((ERROR_COUNT + 1))
    fi
done

echo "azimuth 包测试文件数量: $AZIMUTH_TEST_COUNT"

# 测试 clean_test 包的所有测试文件
echo ""
echo "测试 clean_test 包的所有测试文件..."
cd ../../clean_test/test
CLEAN_TEST_COUNT=0

for file in *.mbt; do
    # 跳过备份文件
    if [[ $file == *.bak* ]]; then
        continue
    fi
    
    CLEAN_TEST_COUNT=$((CLEAN_TEST_COUNT + 1))
    echo "测试文件: $file"
    if ! node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -i ../clean_test.mi "$file" 2>/dev/null; then
        echo "错误: $file 编译失败"
        ERROR_COUNT=$((ERROR_COUNT + 1))
    fi
done

echo "clean_test 包测试文件数量: $CLEAN_TEST_COUNT"

echo ""
echo "全面测试完成！"
echo "总测试文件数量: $((AZIMUTH_TEST_COUNT + CLEAN_TEST_COUNT))"
echo "错误文件数量: $ERROR_COUNT"

if [ $ERROR_COUNT -eq 0 ]; then
    echo "✓ 所有测试文件编译成功！"
    exit 0
else
    echo "✗ 有 $ERROR_COUNT 个文件编译失败"
    exit 1
fi