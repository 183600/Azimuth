#!/bin/bash
# 运行新添加的测试文件

echo "正在运行 azimuth_comprehensive_new_tests.mbt..."
cd /home/runner/work/Azimuth/Azimuth
./moon test 2>&1 | grep -A 20 "azimuth_comprehensive_new_tests"