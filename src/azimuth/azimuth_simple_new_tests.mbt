// Azimuth 简单新增测试用例
// 包含3个基本测试用例，验证核心功能

test "simple_addition_test" {
  // 简单加法测试
  assert_eq(add(2, 3), 5)
  assert_eq(add(0, 0), 0)
  assert_eq(add(-1, 1), 0)
}

test "simple_multiplication_test" {
  // 简单乘法测试
  assert_eq(multiply(3, 4), 12)
  assert_eq(multiply(0, 5), 0)
  assert_eq(multiply(1, 7), 7)
}

test "simple_greet_test" {
  // 简单问候函数测试
  assert_eq_string(greet("World"), "Hello, World!")
  assert_eq_string(greet("MoonBit"), "Hello, MoonBit!")
}