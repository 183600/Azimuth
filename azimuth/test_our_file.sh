#!/bin/bash

echo "检查我们的测试文件..."
echo "文件内容预览："
head -20 azimuth_comprehensive_test_cases.mbt

echo ""
echo "运行测试..."
../moon test 2>&1 | grep -A 10 "Checking azimuth_comprehensive_test_cases"