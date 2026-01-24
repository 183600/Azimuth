// 新增的 MoonBit 测试用例 - 10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_basic_functionality" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(2, -3))
}

test "multiply_basic_functionality" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(2, -3))
}

test "greet_basic_functionality" {
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "add_identity_properties" {
  assert_eq(7, add(7, 0))
  assert_eq(7, add(0, 7))
}

test "multiply_zero_properties" {
  assert_eq(0, multiply(7, 0))
  assert_eq(0, multiply(0, 7))
}

test "divide_with_ceil_negative_numbers" {
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "complex_calculation" {
  let result = add(multiply(3, 4), divide_with_ceil(10, 4))
  assert_eq(15, result)  // 12 + 3 = 15
}

test "greet_unicode_characters" {
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutativity" {
  assert_eq(add(5, 7), add(7, 5))
  assert_eq(multiply(5, 7), multiply(7, 5))
}