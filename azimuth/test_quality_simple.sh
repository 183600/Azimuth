#!/bin/bash
echo "Running quality enhanced test suite..."

# 直接运行测试文件
echo "Checking quality_enhanced_test_suite_new.mbt..."
node moonc.js check -pkg "$(basename $(pwd))" -std-path "../core" test/quality_enhanced_test_suite_new.mbt -i azimuth.mi

if [ $? -eq 0 ]; then
  echo "quality_enhanced_test_suite_new.mbt ... ok"
else
  echo "quality_enhanced_test_suite_new.mbt ... failed"
fi

echo "Test completed."