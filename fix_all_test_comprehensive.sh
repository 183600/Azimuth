#!/bin/bash

# 批量修复所有测试文件中的函数调用
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "=== Fixing azimuth test files ==="
cd "$AZIMUTH_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "Fixing $file..."
    
    # 备份原文件
    cp "$file" "$file.bak5"
    
    # 修复没有前缀的函数调用
    sed -i 's/let result = add(/let result = @azimuth.add(/g' "$file"
    sed -i 's/let result = multiply(/let result = @azimuth.multiply(/g' "$file"
    sed -i 's/let result = greet(/let result = @azimuth.greet(/g' "$file"
    sed -i 's/let result  = add(/let result  = @azimuth.add(/g' "$file"
    sed -i 's/let result  = multiply(/let result  = @azimuth.multiply(/g' "$file"
    sed -i 's/let result  = greet(/let result  = @azimuth.greet(/g' "$file"
    
    # 其他模式
    sed -i 's/if add(/if @azimuth.add(/g' "$file"
    sed -i 's/if multiply(/if @azimuth.multiply(/g' "$file"
    sed -i 's/if greet(/if @azimuth.greet(/g' "$file"
    
    # 替换 azimuth. 为 @azimuth.
    sed -i 's/azimuth\./@azimuth\./g' "$file"
    
    # 替换 clean_test. 为 @clean_test.
    sed -i 's/clean_test\./@clean_test\./g' "$file"
  fi
done

# 修复 clean_test 测试文件
echo ""
echo "=== Fixing clean_test test files ==="
cd "$CLEAN_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "Fixing $file..."
    
    # 备份原文件
    cp "$file" "$file.bak5"
    
    # 修复没有前缀的函数调用
    sed -i 's/let result = add(/let result = @clean_test.add(/g' "$file"
    sed -i 's/let result = multiply(/let result = @clean_test.multiply(/g' "$file"
    sed -i 's/let result = greet(/let result = @clean_test.greet(/g' "$file"
    sed -i 's/let result  = add(/let result  = @clean_test.add(/g' "$file"
    sed -i 's/let result  = multiply(/let result  = @clean_test.multiply(/g' "$file"
    sed -i 's/let result  = greet(/let result  = @clean_test.greet(/g' "$file"
    
    # 其他模式
    sed -i 's/if add(/if @clean_test.add(/g' "$file"
    sed -i 's/if multiply(/if @clean_test.multiply(/g' "$file"
    sed -i 's/if greet(/if @clean_test.greet(/g' "$file"
    
    # 替换 clean_test. 为 @clean_test.
    sed -i 's/clean_test\./@clean_test\./g' "$file"
    
    # 替换 azimuth. 为 @azimuth.
    sed -i 's/azimuth\./@azimuth\./g' "$file"
  fi
done

echo ""
echo "All test files have been fixed!"