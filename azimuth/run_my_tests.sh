#!/bin/bash
echo "Running quality enhanced test suite..."

# 创建一个临时目录
mkdir -p temp_test_dir
cd temp_test_dir

# 复制必要的文件
cp ../lib.mbt .
cp ../azimuth.mi .
cp ../moonc.js .
cp -r ../moonc.assets .

# 创建 test 目录和复制测试文件
mkdir -p test
cp ../test/quality_enhanced_test_suite_new.mbt test/

# 直接运行测试文件
echo "Checking quality_enhanced_test_suite_new.mbt..."
node moonc.js check -pkg "$(basename $(pwd))" -std-path "../../core" test/quality_enhanced_test_suite_new.mbt -i azimuth.mi -i lib.mbt

if [ $? -eq 0 ]; then
  echo "quality_enhanced_test_suite_new.mbt ... ok"
else
  echo "quality_enhanced_test_suite_new.mbt ... failed"
fi

# 返回原目录
cd ..

# 清理
rm -rf temp_test_dir

echo "Test completed."