#!/bin/bash
echo "验证新创建的测试文件..."
echo "文件内容："
cat azimuth_new_standard_test_cases.mbt
echo ""
echo "尝试编译测试文件..."
../moon check azimuth_new_standard_test_cases.mbt