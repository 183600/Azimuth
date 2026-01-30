// 简单的 Azimuth 测试用例 - 5个高质量标准 MoonBit 测试用例

test "unique_addition_test_2024" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "unique_multiplication_test_2024" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(-6, multiply(-2, 3))
}

test "unique_greeting_test_2024" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "unique_division_test_2024" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "unique_subtraction_test_2024" {
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(3, 3))
  assert_eq(-2, subtract(3, 5))
}