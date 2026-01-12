#!/bin/bash

# 综合测试脚本 - 验证所有修复
echo "开始验证所有修复..."

# 生成必要的 .mi 文件
echo "生成 azimuth.mi 文件..."
cd /home/runner/work/Azimuth/Azimuth/src/azimuth
node ../../moonc.js check -pkg azimuth -std-path ../../core -o azimuth.mi lib.mbt

echo "生成 clean_test.mi 文件..."
cd ../clean_test
node ../../moonc.js check -pkg clean_test -std-path ../../core -o clean_test.mi lib.mbt

# 测试一些示例文件
echo "测试 azimuth 包的示例文件..."
cd ../azimuth/test
ERROR_COUNT=0

# 测试几个示例文件
for file in simple_test.mbt lib_test.mbt basic_test.mbt; do
    echo "测试文件: $file"
    if ! node ../../../moonc.js check -pkg azimuth_test -std-path ../../../core -i ../azimuth.mi "$file" 2>/dev/null; then
        echo "错误: $file 编译失败"
        ERROR_COUNT=$((ERROR_COUNT + 1))
    else
        echo "✓ $file 编译成功"
    fi
done

# 测试 clean_test 包的示例文件
echo ""
echo "测试 clean_test 包的示例文件..."
cd ../../clean_test/test

for file in simple_test.mbt lib_test.mbt; do
    echo "测试文件: $file"
    if ! node ../../../moonc.js check -pkg clean_test_test -std-path ../../../core -i ../clean_test.mi "$file" 2>/dev/null; then
        echo "错误: $file 编译失败"
        ERROR_COUNT=$((ERROR_COUNT + 1))
    else
        echo "✓ $file 编译成功"
    fi
done

echo ""
echo "测试完成！"
echo "错误文件数量: $ERROR_COUNT"

if [ $ERROR_COUNT -eq 0 ]; then
    echo "✓ 所有测试文件编译成功！"
    exit 0
else
    echo "✗ 有 $ERROR_COUNT 个文件编译失败"
    exit 1
fi