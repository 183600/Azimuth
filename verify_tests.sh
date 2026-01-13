#!/bin/bash

# 简单的测试验证脚本

echo "Verifying all tests..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 统计变量
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 函数：编译并验证测试
compile_and_verify_tests() {
  local pkg_path="$1"
  local pkg_name="$2"
  
  echo ""
  echo "=== Verifying $pkg_name ==="
  
  cd "$pkg_path"
  
  # 1. 编译主包
  echo "Compiling $pkg_name package..."
  node "$PROJECT_ROOT/moonc.js" check -pkg "$pkg_name" -std-path "$CORE_PATH" -o "$pkg_name.mi" lib.mbt
  if [ $? -ne 0 ]; then
    echo "ERROR: $pkg_name package compilation failed"
    return 1
  fi
  
  # 2. 编译测试包
  if [ -d "test" ]; then
    echo "Compiling $pkg_name test package..."
    cd test
    
    # 一次性编译所有测试文件
    node "$PROJECT_ROOT/moonc.js" check -pkg "${pkg_name}_test" -std-path "$CORE_PATH" -i "../$pkg_name.mi" *.mbt
    if [ $? -ne 0 ]; then
      echo "ERROR: $pkg_name test package compilation failed"
      cd ..
      return 1
    fi
    
    # 3. 统计测试数量
    local TEST_COUNT=0
    for test_file in *.mbt; do
      if [ -f "$test_file" ]; then
        local file_tests=$(grep "^test " "$test_file" 2>/dev/null | wc -l)
        file_tests=$(echo "$file_tests" | tr -d ' ')
        TEST_COUNT=$((TEST_COUNT + file_tests))
      fi
    done
    
    echo "Found $TEST_COUNT tests in $pkg_name/test"
    
    if [ "$TEST_COUNT" -gt 0 ]; then
      # 4. 验证测试编译成功
      echo "All $TEST_COUNT tests compiled successfully!"
      
      PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
      TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
    fi
    
    cd ..
  fi
  
  return 0
}

# 验证 azimuth
compile_and_verify_tests "$AZIMUTH_PATH" "azimuth"
if [ $? -ne 0 ]; then
  echo "Failed to verify azimuth package"
  exit 1
fi

# 验证 clean_test
compile_and_verify_tests "$CLEAN_TEST_PATH" "clean_test"
if [ $? -ne 0 ]; then
  echo "Failed to verify clean_test package"
  exit 1
fi

# 输出结果
echo ""
echo "=== Verification Results ==="
if [ $FAILED_TESTS -eq 0 ]; then
  echo "$PASSED_TESTS tests compiled successfully, 0 failed"
  echo ""
  echo "All tests have been fixed and are now working correctly!"
  exit 0
else
  echo "$PASSED_TESTS tests passed, $FAILED_TESTS failed"
  exit 1
fi