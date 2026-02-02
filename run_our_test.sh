#!/bin/bash

echo "Running our new azimuth_standard_tests.mbt test file..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 运行我们的测试文件
cd "$PROJECT_ROOT"
./moon test -pkg azimuth -pkg-sources azimuth:src/azimuth azimuth_standard_tests.mbt

echo "Test execution completed."