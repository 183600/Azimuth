#!/bin/bash
# 运行用户新创建的标准测试文件

echo "运行用户新创建的标准 MoonBit 测试用例..."
echo "测试文件位置: src/azimuth/test/user_new_standard_tests.mbt"
echo ""

cd /home/runner/work/Azimuth/Azimuth
./moon test src/azimuth/test/user_new_standard_tests.mbt

echo ""
echo "测试完成。"