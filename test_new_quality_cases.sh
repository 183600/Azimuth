#!/bin/bash

# 测试新创建的测试用例
echo "Testing new azimuth quality test cases..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"

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

# 测试新创建的测试文件
echo "Testing new quality test cases..."
node "$PROJECT_ROOT/moonc.js" check -workspace-path "$PROJECT_ROOT" -pkg azimuth -std-path "$CORE_PATH" lib.mbt azimuth_new_quality_test_cases.mbt

if [ $? -eq 0 ]; then
  echo "✅ 新测试用例编译成功！"
  
  # 统计测试数量
  TEST_COUNT=$(grep "^test " "azimuth_new_quality_test_cases.mbt" 2>/dev/null | wc -l)
  TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
  
  echo "📊 测试统计："
  echo "   - 测试用例数量: $TEST_COUNT"
  echo "   - 编译状态: 通过"
  echo ""
  echo "📋 测试用例列表："
  grep "^test " "azimuth_new_quality_test_cases.mbt" | sed 's/test "/  - /' | sed 's/" {$//'
  
  exit 0
else
  echo "❌ 新测试用例编译失败"
  exit 1
fi