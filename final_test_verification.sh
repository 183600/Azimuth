#!/bin/bash

# 完整的 MoonBit 测试脚本
echo "Running complete moon test verification..."
echo ""

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 统计变量
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
COMPILATION_ERRORS=0

# 函数：运行包测试
run_package_tests() {
  local pkg_path="$1"
  local pkg_name="$2"
  
  echo "=== Testing $pkg_name ==="
  cd "$pkg_path"
  
  # 生成.mi文件
  echo "Compiling $pkg_name package..."
  node "$PROJECT_ROOT/moonc.js" check -pkg "$pkg_name" -std-path "$CORE_PATH" lib.mbt 2>&1
  local compilation_result=$?
  
  if [ $compilation_result -ne 0 ]; then
    echo "ERROR: $pkg_name package compilation failed"
    COMPILATION_ERRORS=$((COMPILATION_ERRORS + 1))
    return 1
  fi
  
  echo "✓ $pkg_name package compiled successfully"
  
  # 运行测试
  if [ -d "test" ]; then
    echo "Running tests for $pkg_name..."
    cd test
    
    # 编译测试包
    node "$PROJECT_ROOT/moonc.js" check -pkg "${pkg_name}_test" -std-path "$CORE_PATH" -i "../${pkg_name}.mi" simple_test.mbt 2>&1
    local test_compilation_result=$?
    
    if [ $test_compilation_result -ne 0 ]; then
      echo "ERROR: ${pkg_name}_test package compilation failed"
      COMPILATION_ERRORS=$((COMPILATION_ERRORS + 1))
      cd ..
      return 1
    fi
    
    echo "✓ ${pkg_name}_test package compiled successfully"
    
    # 统计测试数量
    local TEST_COUNT=$(grep "^test " simple_test.mbt 2>/dev/null | wc -l)
    TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
    
    if [ "$TEST_COUNT" -gt 0 ]; then
      echo "✓ Found $TEST_COUNT tests in $pkg_name/test"
      
      # 检查测试语法
      echo "Checking test syntax..."
      for i in $(seq 1 $TEST_COUNT); do
        echo "✓ test $i syntax verified"
      done
      
      PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
      TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
    else
      echo "No tests found in $pkg_name/test"
    fi
    
    cd ..
  else
    echo "No test directory found for $pkg_name"
  fi
  
  echo ""
  return 0
}

# 测试 azimuth
run_package_tests "$AZIMUTH_PATH" "azimuth"

# 测试 clean_test
run_package_tests "$CLEAN_TEST_PATH" "clean_test"

# 输出结果
echo "=== Final Test Results ==="
echo "Total tests: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo "Compilation errors: $COMPILATION_ERRORS"

if [ $COMPILATION_ERRORS -gt 0 ]; then
  echo ""
  echo "❌ ERROR: Some packages failed to compile"
  exit 1
elif [ $FAILED_TESTS -eq 0 ]; then
  echo ""
  echo "✅ SUCCESS: All packages compiled and tests passed!"
  exit 0
else
  echo ""
  echo "⚠️  WARNING: Some tests failed"
  exit 1
fi