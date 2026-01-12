#!/bin/bash

# 检查是否还有未绑定的标识符错误
echo "Checking for unbound identifiers..."

# 搜索 azimuth.azimuth. 模式
echo "Checking for remaining azimuth.azimuth. patterns:"
find /home/runner/work/Azimuth/Azimuth/src -name "*.mbt" -type f -exec grep -l "azimuth\.azimuth\." {} \;

# 搜索 clean_test.clean_test. 模式
echo "Checking for remaining clean_test.clean_test. patterns:"
find /home/runner/work/Azimuth/Azimuth/src -name "*.mbt" -type f -exec grep -l "clean_test\.clean_test\." {} \;

echo "Done checking."