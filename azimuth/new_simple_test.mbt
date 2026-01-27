// 简单的新测试文件
test "simple_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
}

test "simple_greeting_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "simple_division_test" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
}