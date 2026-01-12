#!/bin/bash

# 运行所有测试的脚本
echo "Running all tests to identify issues..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 编译 azimuth 包
echo "Compiling azimuth..."
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: azimuth/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi

# 编译 clean_test 包
echo "Compiling clean_test..."
cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt
if [ $? -ne 0 ]; then
  echo "Error: clean_test/lib.mbt compilation failed"
  exit 1
fi

# 生成 .mi 文件
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt -o clean_test.mi

# 测试 azimuth - 所有测试文件
echo ""
echo "Testing all azimuth tests..."
cd "$AZIMUTH_PATH/test"

# 获取所有 .mbt 文件（排除备份文件）
for file in *.mbt; do
  if [[ "$file" == *.bak ]]; then
    continue
  fi
  
  if [ -f "$file" ]; then
    echo "Checking $file..."
    
    # 编译测试文件
    node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" 2>&1 | tee "test_$file.log"
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
      echo "ERROR: $file compilation failed"
    else
      echo "SUCCESS: $file compilation passed"
    fi
  fi
done

# 测试 clean_test - 所有测试文件
echo ""
echo "Testing all clean_test tests..."
cd "$CLEAN_TEST_PATH/test"

# 获取所有 .mbt 文件（排除备份文件）
for file in *.mbt; do
  if [[ "$file" == *.bak ]]; then
    continue
  fi
  
  if [ -f "$file" ]; then
    echo "Checking $file..."
    
    # 编译测试文件
    node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" 2>&1 | tee "test_$file.log"
    if [ ${PIPESTATUS[0]} -ne 0 ]; then
      echo "ERROR: $file compilation failed"
    else
      echo "SUCCESS: $file compilation passed"
    fi
  fi
done

echo ""
echo "All tests checked. Review test_*.log files for detailed errors."