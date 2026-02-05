#!/bin/bash

echo "运行我们的测试文件..."
../moon test azimuth_comprehensive_test_cases.mbt 2>&1 | grep -A 10 "azimuth_comprehensive_test_cases"