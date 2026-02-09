#!/bin/bash

echo "验证新添加的标准 MoonBit 测试用例..."
echo "================================="

# 进入 azimuth 目录
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查测试文件是否存在
echo "检查测试文件是否存在..."
if [ -f "standard_moonbit_tests.mbt" ]; then
    echo "✓ 测试文件 standard_moonbit_tests.mbt 存在"
else
    echo "✗ 测试文件 standard_moonbit_tests.mbt 不存在"
    exit 1
fi

echo ""
echo "测试文件内容预览..."
echo "=================="
head -20 standard_moonbit_tests.mbt

echo ""
echo "测试文件统计信息..."
echo "=================="
echo "测试用例数量:"
grep -c "test \"" standard_moonbit_tests.mbt

echo ""
echo "断言语句数量:"
grep -c "assert_eq" standard_moonbit_tests.mbt

echo ""
echo "已添加的测试用例列表:"
echo "===================="
grep "test \"" standard_moonbit_tests.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！"