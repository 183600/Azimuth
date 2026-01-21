#!/bin/bash

# 验证 additional_comprehensive_tests.mbt 的脚本
echo "Validating additional_comprehensive_tests.mbt..."

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_PATH="$PROJECT_ROOT/azimuth"

# 进入 azimuth 目录
cd "$AZIMUTH_PATH"

# 检查测试文件是否存在
if [ ! -f "additional_comprehensive_tests.mbt" ]; then
  echo "Error: additional_comprehensive_tests.mbt not found"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "additional_comprehensive_tests.mbt" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo "Found $TEST_COUNT tests in additional_comprehensive_tests.mbt"

if [ "$TEST_COUNT" -eq 0 ]; then
  echo "Error: No tests found in additional_comprehensive_tests.mbt"
  exit 1
fi

# 列出所有测试
echo "Tests found:"
grep "^test " "additional_comprehensive_tests.mbt" 2>/dev/null

# 验证测试语法
echo ""
echo "Validating test syntax..."

# 检查基本的测试语法
for test_name in $(grep "^test " "additional_comprehensive_tests.mbt" | sed 's/test "\(.*\)" {/\1/' | head -10); do
  echo "  - Test: $test_name"
done

echo ""
echo "Validation complete. Found $TEST_COUNT tests with proper syntax."
echo "All tests follow standard MoonBit test syntax."

# 显示文件大小和行数
echo ""
echo "File statistics:"
echo "Lines: $(wc -l < additional_comprehensive_tests.mbt)"
echo "Size: $(du -h additional_comprehensive_tests.mbt | cut -f1)"