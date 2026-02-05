#!/bin/bash

echo "验证标准 MoonBit 测试套件..."
echo "============================"

# 进入 azimuth 目录
cd /home/runner/work/Azimuth/Azimuth/azimuth

# 检查测试文件是否存在
echo "检查测试文件是否存在..."
ls -la test/standard_moonbit_test_suite.mbt

echo ""
echo "测试文件内容预览..."
echo "=================="
cat test/standard_moonbit_test_suite.mbt

echo ""
echo "测试文件统计信息..."
echo "=================="
echo "测试用例数量:"
grep -c "test \"" test/standard_moonbit_test_suite.mbt

echo ""
echo "断言语句数量:"
grep -c "@azimuth.assert_" test/standard_moonbit_test_suite.mbt

echo ""
echo "已添加的测试用例列表:"
echo "===================="
grep "test \"" test/standard_moonbit_test_suite.mbt | sed 's/.*test "\([^"]*\)".*/\1/'

echo ""
echo "验证完成！"