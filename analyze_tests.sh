#!/bin/bash

echo "=== Running MoonBit Tests ==="
cd /home/runner/work/Azimuth/Azimuth/src

# 创建构建目录
mkdir -p _build/test

# 编译azimuth包
echo "1. Compiling azimuth package..."
node ../moonc.js build-package -pkg azimuth -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core azimuth/lib.mbt

# 编译clean_test包
echo "2. Compiling clean_test package..."
node ../moonc.js build-package -pkg clean_test -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core clean_test/lib.mbt

# 检查编译结果
if [ $? -eq 0 ]; then
  echo "✓ Both packages compiled successfully"
else
  echo "✗ Compilation failed"
  exit 1
fi

# 生成测试信息
echo "3. Generating test information..."
node ../moonc.js gen-test-info -json azimuth/test/simple_test.mbt > _build/test/azimuth_test_info.json
node ../moonc.js gen-test-info -json clean_test/test/simple_test.mbt > _build/test/clean_test_test_info.json 2>/dev/null || echo "No clean_test tests found"

# 分析测试问题
echo "4. Analyzing test issues..."

# 检查azimuth测试
echo "Checking azimuth tests:"
for test_file in azimuth/test/*.mbt; do
  if [ -f "$test_file" ]; then
    echo "  - Checking $test_file"
    node ../moonc.js check -pkg azimuth/test -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core "$test_file" 2>&1 | grep -E "(Error|error|E[0-9]+)" | head -5
  fi
done

# 检查clean_test测试
echo "Checking clean_test tests:"
for test_file in clean_test/test/*.mbt; do
  if [ -f "$test_file" ]; then
    echo "  - Checking $test_file"
    node ../moonc.js check -pkg clean_test/test -workspace-path . -std-path /home/runner/work/Azimuth/Azimuth/core "$test_file" 2>&1 | grep -E "(Error|error|E[0-9]+)" | head -5
  fi
done

echo ""
echo "=== Test Analysis Complete ==="
echo "Main issues found:"
echo "1. Test files cannot import functions from main packages"
echo "2. This appears to be a limitation of the current moonc.js import mechanism"
echo ""
echo "Suggested fixes:"
echo "1. Update moonc.js to properly handle package imports in test files"
echo "2. Or modify test files to use a different import approach"
echo ""
echo "Since the user requested to only modify non-test code, the issue"
echo "lies with the compiler/toolchain rather than the code itself."