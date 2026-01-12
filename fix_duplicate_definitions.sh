#!/bin/bash

# 修复azimuth测试文件中的重复定义和类型错误

cd /home/runner/work/Azimuth/Azimuth/src/azimuth/test

for file in *.mbt; do
  if [ -f "$file" ] && [ "$file" != "test_helper.mbt" ]; then
    echo "修复文件: $file"
    
    # 修复函数定义
    sed -i '2s/assert_eq_string/assert_eq/' "$file"
    
    echo "已修复 $file"
  fi
done

echo "所有测试文件的重复定义问题已修复完成"