// 简单的 MoonBit 测试用例
test "simple_addition" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greeting" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "simple_division" {
  assert_eq(3, divide_with_ceil(10, 4))
}

test "simple_zero_cases" {
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(5, 0))
}