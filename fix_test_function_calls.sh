#!/bin/bash

# 批量修复测试文件中的函数调用问题

echo "批量修复测试文件中的函数调用问题..."

# 设置路径
PROJECT_ROOT="/home/runner/work/Azimuth/Azimuth"
AZIMUTH_TEST_PATH="$PROJECT_ROOT/src/azimuth/test"
CLEAN_TEST_PATH="$PROJECT_ROOT/src/clean_test/test"

# 修复 azimuth 测试文件
echo "修复 azimuth 测试文件..."
cd "$AZIMUTH_TEST_PATH"

# 遍历所有 .mbt 文件
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "修复 $file..."
    
    # 使用 sed 进行替换
    # 修复加法运算：将 let result = a + b 替换为 let result = add(a, b)
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*([0-9-]+)[[:space:]]*\+[[:space:]]*([0-9-]+)[[:space:]]*$/\1\2 = add(\3, \4)/' "$file"
    
    # 修复乘法运算：将 let result = a * b 替换为 let result = multiply(a, b)
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*([0-9-]+)[[:space:]]*\*[[:space:]]*([0-9-]+)[[:space:]]*$/\1\2 = multiply(\3, \4)/' "$file"
    
    # 修复字符串拼接：将 "Hello, " + "World" + "!" 替换为 greet("World")
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*"Hello, "[[:space:]]*\+[[:space:]]*"([^"]*)"[[:space:]]*\+[[:space:]]*"!"[[:space:]]*$/\1\2 = greet("\3")/' "$file"
    
    # 修复特殊情况： "Hello, " + "World!" 替换为 greet("World")
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*"Hello, "[[:space:]]*\+[[:space:]]*"([^"]*)"[[:space:]]*$/\1\2 = greet("\3")/' "$file"
  fi
done

# 修复 clean_test 测试文件
echo "修复 clean_test 测试文件..."
cd "$CLEAN_TEST_PATH"

# 遍历所有 .mbt 文件
for file in *.mbt; do
  if [ -f "$file" ]; then
    echo "修复 $file..."
    
    # 使用 sed 进行替换
    # 修复加法运算：将 let result = a + b 替换为 let result = add(a, b)
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*([0-9-]+)[[:space:]]*\+[[:space:]]*([0-9-]+)[[:space:]]*$/\1\2 = add(\3, \4)/' "$file"
    
    # 修复乘法运算：将 let result = a * b 替换为 let result = multiply(a, b)
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*([0-9-]+)[[:space:]]*\*[[:space:]]*([0-9-]+)[[:space:]]*$/\1\2 = multiply(\3, \4)/' "$file"
    
    # 修复字符串拼接：将 "Hello, " + "World" + "!" 替换为 greet("World")
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*"Hello, "[[:space:]]*\+[[:space:]]*"([^"]*)"[[:space:]]*\+[[:space:]]*"!"[[:space:]]*$/\1\2 = greet("\3")/' "$file"
    
    # 修复特殊情况： "Hello, " + "World!" 替换为 greet("World")
    sed -i -E 's/^( *)(let[[:space:]]+[^=]+)[[:space:]]*=[[:space:]]*"Hello, "[[:space:]]*\+[[:space:]]*"([^"]*)"[[:space:]]*$/\1\2 = greet("\3")/' "$file"
  fi
done

echo "所有测试文件修复完成！"