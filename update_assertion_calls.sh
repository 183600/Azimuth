#!/bin/bash

# 更新测试文件中的断言函数调用，添加 @azimuth 前缀

for file in src/azimuth/test/*.mbt; do
  if [ "$file" != "src/azimuth/test/test_helper.mbt" ]; then
    echo "Updating $file..."
    
    # 更新断言函数调用
    sed -i 's/assert_eq(/@azimuth.assert_eq(/g' "$file"
    sed -i 's/assert_eq_string(/@azimuth.assert_eq_string(/g' "$file"
    sed -i 's/assert_true(/@azimuth.assert_true(/g' "$file"
    sed -i 's/assert_false(/@azimuth.assert_false(/g' "$file"
    
    echo "Updated $file"
  fi
done

echo "Done!"