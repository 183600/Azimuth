#!/bin/bash

# 创建一个简单的测试运行脚本
echo "运行标准 MoonBit 测试用例..."

# 使用项目根目录的 moon 命令
cd /home/runner/work/Azimuth/Azimuth

# 检查测试文件是否存在
if [ -f "test/standard_moonbit_tests.mbt" ]; then
    echo "测试文件存在，准备运行..."
    # 尝试直接编译和运行测试
    ./moon test --file test/standard_moonbit_tests.mbt
else
    echo "测试文件不存在！"
fi