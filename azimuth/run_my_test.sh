#!/bin/bash

echo "Running additional_standard_tests.mbt..."

cd /home/runner/work/Azimuth/Azimuth/azimuth

# 尝试直接运行测试文件
./moon test additional_standard_tests.mbt

echo "Test completed."