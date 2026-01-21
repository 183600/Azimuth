#!/bin/bash

# 运行我们的 additional_standard_tests.mbt 测试文件
echo "运行 additional_standard_tests.mbt 测试..."
cd /home/runner/work/Azimuth/Azimuth

# 编译并测试我们的文件
./moon check -pkg-sources azimuth:src/azimuth -pkg azimuth -std-path core src/azimuth/lib.mbt src/azimuth/additional_standard_tests.mbt

if [ $? -eq 0 ]; then
    echo "语法检查通过，运行测试..."
    ./moon test -pkg-sources azimuth:src/azimuth -pkg azimuth -std-path core src/azimuth/lib.mbt src/azimuth/additional_standard_tests.mbt
else
    echo "语法检查失败"
fi