#!/bin/bash

echo "=== Final Verification of MoonBit Test Fixes ==="
echo ""

# 运行测试
echo "1. Running moon test..."
./moon test > test_output_final.log 2>&1
TEST_EXIT_CODE=$?

# 检查测试结果
if [ $TEST_EXIT_CODE -eq 0 ]; then
  echo "✓ Tests passed successfully"
else
  echo "✗ Tests failed with exit code $TEST_EXIT_CODE"
fi

# 检查测试输出
echo ""
echo "2. Checking test output..."
PASSED=$(grep "tests passed" test_output_final.log | tail -1)
FAILED=$(grep "failed" test_output_final.log | tail -1)
echo "Result: $PASSED, $FAILED"

# 检查语法错误
echo ""
echo "3. Checking for syntax errors..."
SYNTAX_ERRORS=$(grep -c "Error:" test_output_final.log)
if [ "$SYNTAX_ERRORS" = "0" ]; then
  echo "✓ No syntax errors found"
else
  echo "✗ Found $SYNTAX_ERRORS syntax errors"
fi

# 检查导入错误
echo ""
echo "4. Checking for import errors..."
IMPORT_ERRORS=$(grep -c "incorrect import" test_output_final.log)
if [ "$IMPORT_ERRORS" = "0" ]; then
  echo "✓ No import errors found"
else
  echo "✗ Found $IMPORT_ERRORS import errors"
fi

# 检查包结构
echo ""
echo "5. Checking package structure..."
if [ -f "src/azimuth/lib.mbt" ] && [ -f "src/clean_test/lib.mbt" ]; then
  echo "✓ Package structure is correct"
else
  echo "✗ Package structure is incorrect"
fi

# 检查测试文件
echo ""
echo "6. Checking test files..."
AZIMUTH_TESTS=$(find src/azimuth/test -name "*.mbt" | wc -l)
CLEAN_TEST_TESTS=$(find src/clean_test/test -name "*.mbt" | wc -l)
echo "Found $AZIMUTH_TESTS test files in azimuth package"
echo "Found $CLEAN_TEST_TESTS test files in clean_test package"

# 总结
echo ""
echo "=== Summary ==="
if [ "$TEST_EXIT_CODE" = "0" ] && [ "$SYNTAX_ERRORS" = "0" ] && [ "$IMPORT_ERRORS" = "0" ]; then
  echo "✓ All issues have been resolved!"
  echo "✓ Moon test is now working correctly"
  exit 0
else
  echo "✗ Some issues still need to be resolved"
  exit 1
fi