#!/bin/bash

echo "Running azimuth comprehensive test cases..."

# 创建一个临时的测试文件，只包含我们的测试用例
cat > temp_test_check.mbt << 'EOF'
// 导入 azimuth 库
use "azimuth"

// 从我们的综合测试用例中复制几个测试来验证
test "extreme_boundary_values" {
  // 测试极值边界情况
  assert_eq(2147483647, add(2147483646, 1))  // 接近 Int 最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近 Int 最小值
  assert_eq(1, add(2147483647, -2147483646))  // 大数相减
  assert_eq(0, multiply(0, 2147483647))  // 零乘以大数
}

test "divide_with_ceil_precision" {
  // 测试除法精度和边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 相等数相除
  assert_eq(1, divide_with_ceil(-1, -1))  // 负数相等相除
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))  // 大数除以1
  assert_eq(1, divide_with_ceil(2147483647, 2147483647))  // 大数自除
  assert_eq(0, divide_with_ceil(0, -1))  // 零除以负数
}

test "string_unicode_handling" {
  // 测试 Unicode 字符串处理
  assert_eq_string("Hello, 🌍!", greet("🌍"))  // Emoji
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))  // 中文字符
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))  // 重音字符
  assert_eq_string("Hello, العربية!", greet("العربية"))  // 阿拉伯文
}
EOF

echo "Created temporary test file. Now running tests..."

# 运行测试
cd /home/runner/work/Azimuth/Azimuth && ./moon test

# 清理临时文件
rm -f /home/runner/work/Azimuth/Azimuth/src/azimuth/temp_test_check.mbt

echo "Test verification completed."