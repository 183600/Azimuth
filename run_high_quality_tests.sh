#!/bin/bash

echo "运行高质量测试用例..."
echo "============================"

# 尝试直接运行测试文件
echo "尝试直接运行 high_quality_test_cases.mbt..."
./moon test src/azimuth/high_quality_test_cases.mbt 2>&1 | head -50

echo ""
echo "尝试查看测试输出..."
./moon test 2>&1 | grep -A 5 "high_quality_test_cases" || echo "未找到 high_quality_test_cases 的输出"

echo ""
echo "检查测试文件是否存在..."
ls -la src/azimuth/high_quality_test_cases.mbt

echo ""
echo "检查 moon.pkg.json 中的配置..."
grep -n "high_quality_test_cases" src/azimuth/moon.pkg.json