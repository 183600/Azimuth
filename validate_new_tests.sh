#!/bin/bash

echo "验证新创建的测试文件..."

# 检查文件是否存在
if [ -f "azimuth_enhanced_comprehensive_test_suite.mbt" ]; then
    echo "✓ 测试文件存在"
else
    echo "✗ 测试文件不存在"
    exit 1
fi

# 检查文件大小
FILE_SIZE=$(wc -l < azimuth_enhanced_comprehensive_test_suite.mbt)
echo "✓ 文件行数: $FILE_SIZE"

# 检查测试用例数量
TEST_COUNT=$(grep -c "^test " azimuth_enhanced_comprehensive_test_suite.mbt)
echo "✓ 测试用例数量: $TEST_COUNT"

# 列出所有测试用例
echo "✓ 测试用例列表:"
grep "^test " azimuth_enhanced_comprehensive_test_suite.mbt | nl

echo "验证完成!"