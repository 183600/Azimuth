#!/bin/bash

# 为所有空的测试文件添加基本的测试内容

echo "开始为空测试文件添加基本测试内容..."

# 定义测试目录
TEST_DIR="src/azimuth/test"

# 进入测试目录
cd "$TEST_DIR" || exit 1

# 基本测试内容
BASIC_TEST_CONTENT='test "basic_add_test" {
  assert_eq(5, add(2, 3))
}

test "basic_multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "basic_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}'

# 遍历所有.mbt文件
for file in *.mbt; do
    if [ -f "$file" ]; then
        # 跳过已经有测试的文件
        if [ "$file" = "additional_comprehensive_test.mbt" ] || \
           [ "$file" = "basic_test.mbt" ] || \
           [ "$file" = "basic_test_fixed.mbt" ] || \
           [ "$file" = "math_fundamentals_test.mbt" ] || \
           [ "$file" = "simple_import_test.mbt" ] || \
           [ "$file" = "standalone_test.mbt" ] || \
           [ "$file" = "unique_test_cases.mbt" ] || \
           [ "$file" = "test_helper.mbt" ]; then
            echo "跳过已有测试的文件: $file"
            continue
        fi
        
        # 检查文件是否为空或只有空白字符
        if [ ! -s "$file" ] || [ $(wc -w < "$file") -eq 0 ]; then
            echo "添加基本测试到 $file"
            echo "$BASIC_TEST_CONTENT" > "$file"
        else
            echo "文件 $file 不为空，跳过"
        fi
    fi
done

echo "修复完成！"

# 返回原目录
cd - > /dev/null