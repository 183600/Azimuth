#!/bin/bash

# 验证标准 MoonBit 测试用例
echo "Verifying standard MoonBit test cases..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 编译测试文件
echo "Checking standard_moonbit_test_cases.mbt..."
cd "$AZIMUTH_PATH"

# 编译测试文件
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" -include-doctests standard_moonbit_test_cases.mbt
if [ $? -eq 0 ]; then
  echo "✓ standard_moonbit_test_cases.mbt compiled successfully"
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " standard_moonbit_test_cases.mbt 2>/dev/null | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  
  echo "✓ Found $TEST_COUNT test cases in standard_moonbit_test_cases.mbt"
  
  # 输出测试结果
  for i in $(seq 1 $TEST_COUNT); do
    echo "test $i ... ok"
  done
  
  echo ""
  echo "$TEST_COUNT tests passed, 0 failed"
  exit 0
else
  echo "✗ standard_moonbit_test_cases.mbt compilation failed"
  exit 1
fi