#!/bin/bash

# 运行additional_standard_moonbit_tests.mbt测试的脚本

echo "正在运行additional_standard_moonbit_tests.mbt测试..."

# 尝试使用moon命令运行测试
cd /home/runner/work/Azimuth/Azimuth
./moon test 2>&1 | grep -A 20 "additional_standard_moonbit_tests.mbt" || echo "未找到additional_standard_moonbit_tests.mbt的测试结果"

echo "测试完成"