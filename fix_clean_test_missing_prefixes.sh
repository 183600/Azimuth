#!/bin/bash

# 修复 clean_test 测试文件中缺少前缀的函数调用
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 clean_test 测试文件
echo "=== Fixing clean_test test files with missing prefixes ==="
cd "$CLEAN_TEST_PATH"

for file in *.mbt; do
  if [ -f "$file" ] && [[ ! "$file" =~ \.log$ ]] && [[ ! "$file" =~ \.bak$ ]] && [ "$file" != "test_functions.mbt" ] && [ "$file" != "test_helper.mbt" ] && [ "$file" != "simple_test.mbt" ] && [ "$file" != "debug_test.mbt" ] && [ "$file" != "lib_test.mbt" ]; then
    echo "Fixing $file..."
    
    # 检查文件是否包含没有前缀的函数调用
    if grep -q "let result  = add(" "$file" || grep -q "let result = add(" "$file"; then
      # 备份原文件
      cp "$file" "$file.bak6"
      
      # 修复没有前缀的函数调用
      sed -i 's/let result  = add(/let result  = @clean_test.add(/g' "$file"
      sed -i 's/let result  = multiply(/let result  = @clean_test.multiply(/g' "$file"
      sed -i 's/let result  = greet(/let result  = @clean_test.greet(/g' "$file"
      sed -i 's/let result = add(/let result = @clean_test.add(/g' "$file"
      sed -i 's/let result = multiply(/let result = @clean_test.multiply(/g' "$file"
      sed -i 's/let result = greet(/let result = @clean_test.greet(/g' "$file"
      
      # 其他模式
      sed -i 's/if add(/if @clean_test.add(/g' "$file"
      sed -i 's/if multiply(/if @clean_test.multiply(/g' "$file"
      sed -i 's/if greet(/if @clean_test.greet(/g' "$file"
    fi
  fi
done

echo ""
echo "All clean_test test files have been fixed!"