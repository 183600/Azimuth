// 简洁的 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_positive_numbers" {
  @azimuth.assert_eq(10, @azimuth.add(7, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "multiply_basic" {
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 10))
}

test "multiply_with_negative" {
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_simple" {
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
}

test "greet_empty_string" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_positive" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
}

test "divide_with_ceil_zero_divisor" {
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative" {
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
}