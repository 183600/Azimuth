// 新增的 MoonBit 测试用例
// 测试 azimuth 库的各种功能

test "add_with_negative_and_positive" {
  // 测试正负数相加
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
}

test "add_with_large_numbers" {
  // 测试大数相加
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
  @azimuth.assert_eq(0, @azimuth.add(1500000, -1500000))
}

test "multiply_with_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "multiply_with_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(-1000, -1000))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "greet_with_numbers" {
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的特殊情况
  @azimuth.assert_eq(-10, @azimuth.multiply(10, -1))
  @azimuth.assert_eq(10, @azimuth.multiply(-10, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1)) // 最小值特殊情况
}

test "add_min_value_cases" {
  // 测试最小值的加法情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}