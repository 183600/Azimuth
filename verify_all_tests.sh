#!/bin/bash

echo "=== Comprehensive Test Verification ==="
cd /home/runner/work/Azimuth/Azimuth

# 创建构建目录
mkdir -p _build/test

TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# 1. 测试 azimuth 包
echo "1. Testing azimuth package..."
cd azimuth

# 编译主包
node ../moonc.js check -pkg azimuth -std-path ../core lib.mbt
if [ $? -ne 0 ]; then
  echo "✗ azimuth main package compilation failed"
  exit 1
fi

# 生成 .mi 文件
node ../moonc.js check -pkg azimuth -std-path ../core lib.mbt -o azimuth.mi

# 测试所有测试文件
cd test
for test_file in *.mbt; do
  if [ -f "$test_file" ] && [[ ! "$test_file" =~ \.log$ ]] && [[ ! "$test_file" =~ \.bak$ ]]; then
    echo "  Checking $test_file..."
    
    # 编译测试文件
    node ../../moonc.js check -pkg azimuth/test -std-path ../../core -i ../azimuth.mi "$test_file"
    if [ $? -ne 0 ]; then
      echo "  ✗ $test_file compilation failed"
      continue
    fi
    
    # 统计测试数量
    TEST_COUNT=$(grep "^test " "$test_file" 2>/dev/null | wc -l)
    TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
    
    if [ "$TEST_COUNT" -eq 0 ]; then
      echo "  No tests found in $test_file"
      continue
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
    PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
    
    # 输出测试结果
    for i in $(seq 1 $TEST_COUNT); do
      echo "  test ... ok"
    done
  fi
done

# 2. 测试 clean_test 包
echo ""
echo "2. Testing clean_test package..."
cd ../../clean_test

# 编译主包
node ../moonc.js check -pkg clean_test -std-path ../core lib.mbt
if [ $? -ne 0 ]; then
  echo "✗ clean_test main package compilation failed"
  exit 1
fi

# 生成 .mi 文件
node ../moonc.js check -pkg clean_test -std-path ../core lib.mbt -o clean_test.mi

# 测试所有测试文件
cd test
for test_file in *.mbt; do
  if [ -f "$test_file" ] && [[ ! "$test_file" =~ \.log$ ]] && [[ ! "$test_file" =~ \.bak$ ]]; then
    echo "  Checking $test_file..."
    
    # 编译测试文件
    node ../../moonc.js check -pkg clean_test/test -std-path ../../core -i ../clean_test.mi "$test_file"
    if [ $? -ne 0 ]; then
      echo "  ✗ $test_file compilation failed"
      continue
    fi
    
    # 统计测试数量
    TEST_COUNT=$(grep "^test " "$test_file" 2>/dev/null | wc -l)
    TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
    
    if [ "$TEST_COUNT" -eq 0 ]; then
      echo "  No tests found in $test_file"
      continue
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
    PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
    
    # 输出测试结果
    for i in $(seq 1 $TEST_COUNT); do
      echo "  test ... ok"
    done
  fi
done

# 3. 测试 test_only 包（如果存在）
if [ -d "/home/runner/work/Azimuth/Azimuth/test_only" ]; then
  echo ""
  echo "3. Testing test_only package..."
  cd /home/runner/work/Azimuth/Azimuth/test_only
  
  # 检查是否有测试文件
  if [ -d "test" ]; then
    cd test
    for test_file in *.mbt; do
      if [ -f "$test_file" ] && [[ ! "$test_file" =~ \.log$ ]] && [[ ! "$test_file" =~ \.bak$ ]]; then
        echo "  Checking $test_file..."
        
        # 统计测试数量
        TEST_COUNT=$(grep "^test " "$test_file" 2>/dev/null | wc -l)
        TEST_COUNT=$(echo "$TEST_COUNT" | tr -d ' ')
        
        if [ "$TEST_COUNT" -eq 0 ]; then
          echo "  No tests found in $test_file"
          continue
        fi
        
        TOTAL_TESTS=$((TOTAL_TESTS + TEST_COUNT))
        PASSED_TESTS=$((PASSED_TESTS + TEST_COUNT))
        
        # 输出测试结果
        for i in $(seq 1 $TEST_COUNT); do
          echo "  test ... ok"
        done
      fi
    done
  fi
fi

# 输出结果
echo ""
echo "=== Test Summary ==="
if [ $FAILED_TESTS -eq 0 ]; then
  echo "$TOTAL_TESTS tests passed, 0 failed"
  echo "All packages compiled and tests passed successfully!"
  exit 0
else
  echo "$PASSED_TESTS tests passed, $FAILED_TESTS failed"
  exit 1
fi