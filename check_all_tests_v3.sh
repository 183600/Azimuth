#!/bin/bash

# 检查所有测试文件的编译状态 - 只检查真正的错误
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CORE_PATH="$PROJECT_ROOT/core"
AZIMUTH_PATH="$PROJECT_ROOT/src/azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test"

# 生成 .mi 文件
cd "$AZIMUTH_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg azimuth -std-path "$CORE_PATH" lib.mbt -o azimuth.mi 2>/dev/null

cd "$CLEAN_TEST_PATH"
node "$PROJECT_ROOT/moonc.js" check -pkg clean_test -std-path "$CORE_PATH" lib.mbt -o clean_test.mi 2>/dev/null

# 检查 azimuth 测试文件
echo "=== Checking azimuth test files ==="
cd "$AZIMUTH_PATH/test"
FAILED_FILES=""
TOTAL_FILES=0
PASSED_FILES=0

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    TOTAL_FILES=$((TOTAL_FILES + 1))
    echo -n "Checking $file... "
    
    # 运行编译检查，只检查退出码
    node "$PROJECT_ROOT/moonc.js" check -pkg azimuth_test -std-path "$CORE_PATH" -i ../azimuth.mi "$file" >/dev/null 2>&1
    if [ $? -eq 0 ]; then
      echo "OK"
      PASSED_FILES=$((PASSED_FILES + 1))
    else
      echo "FAILED"
      FAILED_FILES="$FAILED_FILES $file"
    fi
  fi
done

echo ""
echo "Azimuth test files: $PASSED_FILES/$TOTAL_FILES passed"
if [ ! -z "$FAILED_FILES" ]; then
  echo "Failed files:$FAILED_FILES"
fi

# 检查 clean_test 测试文件
echo ""
echo "=== Checking clean_test test files ==="
cd "$CLEAN_TEST_PATH/test"
FAILED_FILES=""
TOTAL_FILES=0
PASSED_FILES=0

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]]; then
    TOTAL_FILES=$((TOTAL_FILES + 1))
    echo -n "Checking $file... "
    
    # 运行编译检查，只检查退出码
    node "$PROJECT_ROOT/moonc.js" check -pkg clean_test_test -std-path "$CORE_PATH" -i ../clean_test.mi "$file" >/dev/null 2>&1
    if [ $? -eq 0 ]; then
      echo "OK"
      PASSED_FILES=$((PASSED_FILES + 1))
    else
      echo "FAILED"
      FAILED_FILES="$FAILED_FILES $file"
    fi
  fi
done

echo ""
echo "Clean_test test files: $PASSED_FILES/$TOTAL_FILES passed"
if [ ! -z "$FAILED_FILES" ]; then
  echo "Failed files:$FAILED_FILES"
fi