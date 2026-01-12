#!/bin/bash

# 更新测试文件中的断言函数调用，添加 @clean_test 前缀

for file in src/clean_test/test/*.mbt; do
  if [ "$file" != "src/clean_test/test/test_helper.mbt" ]; then
    echo "Updating $file..."
    
    # 更新断言函数调用
    sed -i 's/assert_eq(/@clean_test.assert_eq(/g' "$file"
    sed -i 's/assert_eq_string(/@clean_test.assert_eq_string(/g' "$file"
    sed -i 's/assert_true(/@clean_test.assert_true(/g' "$file"
    sed -i 's/assert_false(/@clean_test.assert_false(/g' "$file"
    
    echo "Updated $file"
  fi
done

echo "Done!"