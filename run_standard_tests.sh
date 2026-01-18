#!/bin/bash
# 运行我们的标准测试文件

echo "运行标准 MoonBit 测试用例..."

cd /home/runner/work/Azimuth/Azimuth
./moon test src/azimuth/standard_moonbit_tests.mbt

echo "测试完成。"