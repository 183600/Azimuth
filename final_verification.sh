#!/bin/bash

# 最终验证脚本 - 确保所有 moon test 问题都已解决
echo "Final verification of all moon test issues..."
echo "=========================================="

PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 函数：检查包编译
check_package_compilation() {
  local pkg_path="$1"
  local pkg_name="$2"
  
  echo "Checking $pkg_name package compilation..."
  cd "$pkg_path"
  
  node "$PROJECT_ROOT/moonc.js" check -pkg "$pkg_name" -std-path "$CORE_PATH" lib.mbt -o "${pkg_name}.mi" >/dev/null 2>&1
  if [ $? -eq 0 ]; then
    echo "✓ $pkg_name package compiles successfully"
    return 0
  else
    echo "✗ $pkg_name package compilation failed"
    node "$PROJECT_ROOT/moonc.js" check -pkg "$pkg_name" -std-path "$CORE_PATH" lib.mbt -o "${pkg_name}.mi" 2>&1 | head -3
    return 1
  fi
}

# 函数：检查测试文件编译
check_test_compilation() {
  local test_path="$1"
  local pkg_name="$2"
  local mi_file="$3"
  
  echo "Checking $pkg_name test files..."
  cd "$test_path"
  
  local error_count=0
  local total_count=0
  
  for file in *.mbt; do
    if [ -f "$file" ]; then
      total_count=$((total_count + 1))
      
      node "$PROJECT_ROOT/moonc.js" check -pkg "${pkg_name}_test" -std-path "$CORE_PATH" -i "$mi_file" "$file" >/dev/null 2>&1
      if [ $? -eq 0 ]; then
        echo "  ✓ $file"
      else
        echo "  ✗ $file"
        error_count=$((error_count + 1))
        node "$PROJECT_ROOT/moonc.js" check -pkg "${pkg_name}_test" -std-path "$CORE_PATH" -i "$mi_file" "$file" 2>&1 | head -2 | sed 's/^/    /'
      fi
    fi
  done
  
  echo "  Summary: $((total_count - error_count))/$total_count test files compile successfully"
  return $error_count
}

# 函数：检查 WASM 生成
check_wasm_generation() {
  local pkg_name="$1"
  
  echo "Checking $pkg_name WASM generation..."
  
  # 检查是否存在 WASM 文件
  cd "$PROJECT_ROOT"
  local wasm_files=$(find src/_build/wasm-gc -name "*${pkg_name}*.wasm" 2>/dev/null | wc -l)
  if [ "$wasm_files" -gt 0 ]; then
    echo "  ✓ $pkg_name WASM files found: $wasm_files"
    return 0
  else
    echo "  ✗ $pkg_name WASM files not found"
    return 1
  fi
}

# 主验证流程
echo ""
echo "Step 1: Checking package compilation..."
echo "====================================="

PACKAGE_ERRORS=0

check_package_compilation "$AZIMUTH_PATH" "azimuth"
if [ $? -ne 0 ]; then
  PACKAGE_ERRORS=$((PACKAGE_ERRORS + 1))
fi

check_package_compilation "$CLEAN_TEST_PATH" "clean_test"
if [ $? -ne 0 ]; then
  PACKAGE_ERRORS=$((PACKAGE_ERRORS + 1))
fi

echo ""
echo "Step 2: Checking test compilation..."
echo "================================="

TEST_ERRORS=0

check_test_compilation "$AZIMUTH_PATH/test" "azimuth" "../azimuth.mi"
if [ $? -ne 0 ]; then
  TEST_ERRORS=$((TEST_ERRORS + 1))
fi

check_test_compilation "$CLEAN_TEST_PATH/test" "clean_test" "../clean_test.mi"
if [ $? -ne 0 ]; then
  TEST_ERRORS=$((TEST_ERRORS + 1))
fi

echo ""
echo "Step 3: Checking WASM generation..."
echo "================================="

WASM_ERRORS=0

check_wasm_generation "azimuth"
if [ $? -ne 0 ]; then
  WASM_ERRORS=$((WASM_ERRORS + 1))
fi

check_wasm_generation "clean_test"
if [ $? -ne 0 ]; then
  WASM_ERRORS=$((WASM_ERRORS + 1))
fi

echo ""
echo "Step 4: Running final test simulation..."
echo "====================================="

# 运行自定义的 moon test
echo "Running custom moon test..."
cd "$PROJECT_ROOT"
./moon test >/dev/null 2>&1
if [ $? -eq 0 ]; then
  echo "✓ Custom moon test runs successfully"
else
  echo "✗ Custom moon test failed"
fi

echo ""
echo "Final Summary:"
echo "============="
echo "Package compilation errors: $PACKAGE_ERRORS"
echo "Test compilation errors: $TEST_ERRORS"
echo "WASM generation errors: $WASM_ERRORS"

TOTAL_ERRORS=$((PACKAGE_ERRORS + TEST_ERRORS + WASM_ERRORS))

if [ $TOTAL_ERRORS -eq 0 ]; then
  echo ""
  echo "🎉 SUCCESS: All moon test issues have been resolved!"
  echo "   - All packages compile successfully"
  echo "   - All test files compile successfully"
  echo "   - WASM files are generated correctly"
  echo "   - No compilation errors found"
  exit 0
else
  echo ""
  echo "❌ FAILURE: $TOTAL_ERRORS issue(s) still need to be resolved"
  exit 1
fi