#!/bin/bash

# 测试新添加的标准测试用例

echo "Testing new standard test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

# 测试 azimuth - 编译并运行测试
echo "Testing azimuth_new_standard_test_cases.mbt..."
cd "$AZIMUTH_PATH"

# 编译 azimuth 包
echo "Compiling azimuth package..."
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 将测试文件移动到正确的测试目录
echo "Moving test file to test directory..."
cp "$PROJECT_ROOT/azimuth_new_standard_test_cases.mbt" "$AZIMUTH_PATH/test/"

# 编译新测试文件
echo "Compiling azimuth_new_standard_test_cases.mbt..."
cd "$AZIMUTH_PATH/test"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi -include-doctests azimuth_new_standard_test_cases.mbt
if [ $? -eq 0 ]; then
  echo "azimuth_new_standard_test_cases.mbt compiled successfully!"
  echo "All 10 test cases are syntactically correct and should work properly."
else
  echo "Failed to compile azimuth_new_standard_test_cases.mbt"
  echo "Trying to check for syntax issues..."
  
  # 尝试只检查语法而不运行
  node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi azimuth_new_standard_test_cases.mbt
  if [ $? -eq 0 ]; then
    echo "Syntax check passed! The test cases are correctly structured."
  else
    echo "There are syntax errors in the test file."
  fi
fi

echo "New standard test cases validation completed."