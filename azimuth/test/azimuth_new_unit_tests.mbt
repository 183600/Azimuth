// 新增的 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_basic_functionality" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
}

test "multiply_basic_functionality" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "greet_basic_functionality" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_basic" {
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "add_identity_properties" {
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
}

test "multiply_zero_properties" {
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
}

test "divide_with_ceil_negative_numbers" {
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "complex_calculation" {
  let result = @azimuth.add(@azimuth.multiply(3, 4), @azimuth.divide_with_ceil(10, 4))
  @azimuth.assert_eq(15, result)  // 12 + 3 = 15
}

test "greet_unicode_characters" {
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

test "mathematical_commutativity" {
  @azimuth.assert_eq(@azimuth.add(5, 7), @azimuth.add(7, 5))
  @azimuth.assert_eq(@azimuth.multiply(5, 7), @azimuth.multiply(7, 5))
}