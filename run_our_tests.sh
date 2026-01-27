#!/bin/bash

echo "运行我们新创建的测试文件..."
echo "================================"

cd /home/runner/work/Azimuth/Azimuth

# 尝试运行测试
./moon test 2>&1 | grep -A 5 "基本加法运算测试\|基本乘法运算测试\|问候函数测试\|向上取整除法正数测试\|向上取整除法负数测试\|除零错误处理测试\|减法运算测试\|数学交换律测试\|复杂运算组合测试\|实际应用场景测试" || echo "未找到我们的测试用例"

echo ""
echo "检查测试文件是否存在..."
ls -la azimuth/standard_azimuth_test_cases.mbt

echo ""
echo "检查测试文件是否在moon.pkg.json中..."
grep -n "standard_azimuth_test_cases.mbt" azimuth/moon.pkg.json