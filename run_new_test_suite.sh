#!/bin/bash
# 运行新的测试套件

echo "运行新的 MoonBit 测试套件..."

cd /home/runner/work/Azimuth/Azimuth
./moon test src/azimuth/azimuth_new_test_suite.mbt

echo "测试完成。"