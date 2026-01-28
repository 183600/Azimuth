// 简单测试文件
test "simple_addition_test" {
  assert_eq(5, add(2, 3))
  assert_eq(10, add(5, 5))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
}

test "simple_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}