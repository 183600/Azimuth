#!/bin/bash
echo "Running simple quality test suite..."

# 创建一个临时目录
mkdir -p temp_test_dir
cd temp_test_dir

# 复制必要的文件
cp ../lib.mbt .
cp ../azimuth.mi .

# 创建 test 目录和复制测试文件
mkdir -p test
cp ../test/quality_enhanced_test_suite_new.mbt test/

# 运行测试
echo "Executing tests..."
../moon_test_correct

# 返回原目录
cd ..

# 清理
rm -rf temp_test_dir

echo "Test completed."