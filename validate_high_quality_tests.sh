#!/bin/bash

# 验证新创建的高质量测试套件
echo "验证高质量测试套件..."

# 检查文件是否存在
if [ -f "/home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_test_suite.mbt" ]; then
    echo "✓ 测试文件存在"
else
    echo "✗ 测试文件不存在"
    exit 1
fi

# 统计测试用例数量
test_count=$(grep -c "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_test_suite.mbt)
echo "✓ 测试用例数量: $test_count"

# 检查测试用例名称
echo "✓ 测试用例列表:"
grep "^test " /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_test_suite.mbt | sed 's/test "/- /' | sed 's/" {/:/'

# 检查文件大小
file_size=$(du -h /home/runner/work/Azimuth/Azimuth/azimuth/azimuth_high_quality_test_suite.mbt | cut -f1)
echo "✓ 文件大小: $file_size"

# 检查文件是否已添加到测试配置
if grep -q "azimuth_high_quality_test_suite.mbt" /home/runner/work/Azimuth/Azimuth/azimuth/moon.pkg.json; then
    echo "✓ 测试文件已添加到moon.pkg.json"
else
    echo "✗ 测试文件未添加到moon.pkg.json"
fi

echo "验证完成!"