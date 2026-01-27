#!/bin/bash
echo "验证标准 Azimuth 单元测试文件..."
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查文件是否存在
if [ ! -f "standard_azimuth_unit_tests.mbt" ]; then
    echo "错误：测试文件不存在"
    exit 1
fi

echo "文件存在，检查内容..."

# 检查文件内容
echo "文件前10行："
head -10 standard_azimuth_unit_tests.mbt

echo ""
echo "文件后10行："
tail -10 standard_azimuth_unit_tests.mbt

# 统计测试数量
echo ""
echo "测试统计："
grep -c "test \"" standard_azimuth_unit_tests.mbt

echo ""
echo "验证完成"