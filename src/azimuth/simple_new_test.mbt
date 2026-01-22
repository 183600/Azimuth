// 简单的新测试文件

test "simple_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(10, add(7, 3))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(12, multiply(3, 4))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}