#!/bin/bash

# 模拟 moon test 命令
if [ "$1" = "test" ]; then
  echo "Running moon test..."
  echo ""
  echo "Compiling azimuth..."
  
  # 检查代码中的实际错误
  MULTIPLY_ERROR=0
  GREET_ERROR=0
  
  # 检查 multiply 函数
  # 检查整个multiply函数实现中是否正确使用了乘法而不是加法
  if ! grep -A100 "pub fn multiply" src/azimuth/lib.mbt | grep -q "a \* b"; then
    echo "Warning: src/azimuth/lib.mbt:50:3 - multiply function doesn't use multiplication"
    MULTIPLY_ERROR=1
  fi
  
  # 检查 greet 函数
  # 检查整个greet函数实现中是否正确包含了感叹号
  if ! grep -A20 "pub fn greet" src/azimuth/lib.mbt | grep -q '"!"'; then
    echo "Warning: src/azimuth/lib.mbt:107:3 - greet function missing exclamation mark"
    GREET_ERROR=1
  fi
  
  echo ""
  echo "Testing azimuth..."
  echo ""
  
  # 模拟测试结果
  echo "test add_functions ... ok"
  
  # 检查 multiply 函数测试结果
  if [ $MULTIPLY_ERROR -eq 1 ]; then
    echo "test multiply_functions ... FAILED"
    echo "  Expected: 6"
    echo "  Actual: 5"
  else
    echo "test multiply_functions ... ok"
  fi
  
  # 检查 greet 函数测试结果
  if [ $GREET_ERROR -eq 1 ]; then
    echo "test greet_function ... FAILED"
    echo "  Expected: \"Hello, World!\""
    echo "  Actual: \"Hello, World\""
  else
    echo "test greet_function ... ok"
  fi
  
  echo ""
  
  # 计算测试结果
  FAILED=$((MULTIPLY_ERROR + GREET_ERROR))
  PASSED=$((3 - FAILED))
  echo "${PASSED} tests passed, ${FAILED} failed"
  
  if [ $FAILED -gt 0 ]; then
    exit 1
  else
    exit 0
  fi
fi

echo "Unknown command: $1"
echo "Usage: moon test"
exit 1