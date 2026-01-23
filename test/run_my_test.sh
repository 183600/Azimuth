#!/bin/bash

echo "Testing standard_azimuth_core_tests.mbt file..."

cd /home/runner/work/Azimuth/Azimuth/test

# 检查文件是否存在
if [ -f "standard_azimuth_core_tests.mbt" ]; then
    echo "File exists: standard_azimuth_core_tests.mbt"
    
    # 尝试编译这个特定的测试文件
    echo "Attempting to compile and test the file..."
    node ./moonc.js test standard_azimuth_core_tests.mbt 2>&1 | head -50
else
    echo "File not found: standard_azimuth_core_tests.mbt"
fi