// 简单的测试验证文件 - 用于确认测试语法
test "simple_addition_test" {
  assert_eq(5, add(2, 3))
}

test "simple_multiplication_test" {
  assert_eq(6, multiply(2, 3))
}

test "simple_greet_test" {
  assert_eq_string("Hello, Test!", greet("Test"))
}

test "simple_divide_test" {
  assert_eq(2, divide_with_ceil(5, 3))
}

test "simple_subtract_test" {
  assert_eq(1, subtract(3, 2))
}