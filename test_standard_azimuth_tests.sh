#!/bin/bash
# 测试新创建的 standard_azimuth_tests.mbt 文件

echo "测试 standard_azimuth_tests.mbt 文件..."

# 检查文件是否存在
if [ ! -f "azimuth/standard_azimuth_tests.mbt" ]; then
    echo "错误: standard_azimuth_tests.mbt 文件不存在"
    exit 1
fi

echo "文件存在，开始编译和测试..."

# 尝试编译
cd /home/runner/work/Azimuth/Azimuth
./moon check azimuth/standard_azimuth_tests.mbt

if [ $? -eq 0 ]; then
    echo "编译成功!"
else
    echo "编译失败!"
    exit 1
fi

echo "测试完成!"