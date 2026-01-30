#!/bin/bash

echo "Testing azimuth_standard_tests.mbt..."

cd src/azimuth

# 尝试直接运行moon test并过滤输出
../../moon test 2>&1 | grep -A 15 "azimuth_standard_tests.mbt"

echo "Test completed."