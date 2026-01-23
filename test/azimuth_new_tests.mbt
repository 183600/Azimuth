// Azimuth 新增测试用例
test "azimuth_add_test" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
}

test "azimuth_multiply_test" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
}

test "azimuth_greet_test" {
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
}

test "azimuth_divide_with_ceil_test" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
}

test "azimuth_complex_calculation" {
  let sum = @azimuth.add(5, 3)
  let product = @azimuth.multiply(sum, 2)
  let result = @azimuth.divide_with_ceil(product, 4)
  @azimuth.assert_eq(4, result)
}

test "azimuth_negative_numbers" {
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
}

test "azimuth_zero_operations" {
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "azimuth_large_numbers" {
  @azimuth.assert_eq(2000, @azimuth.add(1000, 1000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(1000, @azimuth.divide_with_ceil(10000, 10))
}

test "azimuth_unicode_greeting" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "azimuth_mathematical_properties" {
  // 测试交换律
  let a = 7
  let b = 13
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}