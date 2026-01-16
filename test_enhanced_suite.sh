#!/bin/bash

# 测试新创建的 azimuth_enhanced_test_suite.mbt 文件
echo "Testing azimuth_enhanced_test_suite.mbt..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
NEW_TEST_FILE="$PROJECT_ROOT/azimuth_enhanced_test_suite.mbt"

# 检查文件是否存在
if [ ! -f "$NEW_TEST_FILE" ]; then
  echo "Error: Test file $NEW_TEST_FILE not found"
  exit 1
fi

# 统计测试数量
TEST_COUNT=$(grep "^test " "$NEW_TEST_FILE" 2>/dev/null | wc -l)
TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')

echo ""
echo "Test file found: $NEW_TEST_FILE"
echo "Found $TEST_COUNT test cases in the enhanced test suite"
echo ""
echo "Test cases included:"
grep "^test " "$NEW_TEST_FILE" 2>/dev/null | sed 's/test "//' | sed 's/".*$/"/' | sed 's/^/- /'

echo ""
echo "Test syntax validation:"
echo "Checking for proper test syntax..."

# 检查测试语法
SYNTAX_ERRORS=0

# 检查每个测试是否有正确的结构
while IFS= read -r line; do
  if [[ $line =~ ^test\ \"(.+)\"\ \{ ]]; then
    test_name="${BASH_REMATCH[1]}"
    echo "- Found test: $test_name"
    
    # 检查是否有对应的 assert_eq 语句
    test_content=$(sed -n "/^test \"$test_name\" {/,/^}/p" "$NEW_TEST_FILE")
    assert_count=$(echo "$test_content" | grep -c "assert_eq")
    
    if [ "$assert_count" -gt 0 ]; then
      echo "  ✓ Contains $assert_count assertion(s)"
    else
      echo "  ✗ No assertions found"
      SYNTAX_ERRORS=$((SYNTAX_ERRORS + 1))
    fi
  fi
done < "$NEW_TEST_FILE"

# 检查是否有未闭合的大括号
OPEN_BRACES=$(grep -o "{" "$NEW_TEST_FILE" | wc -l)
CLOSE_BRACES=$(grep -o "}" "$NEW_TEST_FILE" | wc -l)

if [ "$OPEN_BRACES" -eq "$CLOSE_BRACES" ]; then
  echo "✓ All braces are properly closed"
else
  echo "✗ Brace mismatch: $OPEN_BRACES open, $CLOSE_BRACES closed"
  SYNTAX_ERRORS=$((SYNTAX_ERRORS + 1))
fi

# 检查是否使用了正确的函数调用
AZIMUTH_CALLS=$(grep -c "azimuth::" "$NEW_TEST_FILE")
echo "✓ Found $AZIMUTH_CALLS azimuth function calls"

echo ""
if [ "$SYNTAX_ERRORS" -eq 0 ]; then
  echo "✓ All syntax checks passed!"
  echo "✓ Test suite is ready for execution"
  exit 0
else
  echo "✗ Found $SYNTAX_ERRORS syntax error(s)"
  exit 1
fi