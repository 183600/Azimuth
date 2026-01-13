#!/bin/bash

# 最终修复测试文件

echo "Final fix for test files..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 1. 首先修复 test_helpers.mbt 中的 panic 调用
echo "Fixing panic calls in test_helpers.mbt..."
cd "$AZIMUTH_TEST_PATH"

if [ -f "test_helpers.mbt" ]; then
  sed -i 's/panic()/@builtin.panic()/g' test_helpers.mbt
  echo "  Fixed panic calls in azimuth/test_helpers.mbt"
fi

cd "$CLEAN_TEST_TEST_PATH"

if [ -f "test_helpers.mbt" ]; then
  sed -i 's/panic()/@builtin.panic()/g' test_helpers.mbt
  echo "  Fixed panic calls in clean_test/test_helpers.mbt"
fi

# 2. 修复其他测试文件中的 panic 调用
echo "Fixing panic calls in other test files..."

cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "test_helpers.mbt" ] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_functions_def.mbt" ] && [ "$file" != "test_helper.mbt" ] && [ "$file" != "test_shared.mbt" ]; then
    sed -i 's/panic()/@builtin.panic()/g' "$file"
    echo "  Fixed panic calls in $file"
  fi
done

cd "$CLEAN_TEST_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "test_helpers.mbt" ]; then
    sed -i 's/panic()/@builtin.panic()/g' "$file"
    echo "  Fixed panic calls in $file"
  fi
done

# 3. 删除多余的 init 测试
echo "Removing extra init tests..."

cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ]; then
    # 删除多余的 azimuth_init 测试
    sed -i '/^test "azimuth_init" { }$/d' "$file"
    # 删除空行
    sed -i '/^$/d' "$file"
  fi
done

cd "$CLEAN_TEST_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ]; then
    # 删除多余的 clean_test_init 测试
    sed -i '/^test "clean_test_init" { }$/d' "$file"
    # 删除空行
    sed -i '/^$/d' "$file"
  fi
done

echo "Final fixes complete."