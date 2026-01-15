// 简单的 MoonBit 测试用例

test "simple_add_test" {
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(0, azimuth::add(-5, 5))
}

test "simple_multiply_test" {
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(-12, azimuth::multiply(-3, 4))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "math_expression_test" {
  let result = azimuth::multiply(azimuth::add(2, 3), 4)
  assert_eq(20, result)
}

test "practical_test" {
  let price = 199
  let quantity = 3
  let total = azimuth::multiply(price, quantity)
  assert_eq(597, total)
}

test "overflow_test" {
  let max_val = 2147483647
  assert_eq(max_val, azimuth::add(max_val, 1))
}

test "boundary_test" {
  let min_val = -2147483648
  assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "string_test" {
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
  assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
}

test "complex_test" {
  let a = 10
  let b = 5
  let c = 2
  let result = azimuth::add(azimuth::multiply(a, b), c)
  assert_eq(52, result)
}

test "assert_test" {
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
}