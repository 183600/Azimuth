// 简单的新测试文件
test "simple_add_test" {
  assert_eq(5, add(2, 3))
}

// 用户添加的测试用例
test "user_subtract_test" {
  assert_eq(7, subtract(10, 3))
}

test "user_multiply_test" {
  assert_eq(20, multiply(4, 5))
}

test "user_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}

test "user_divide_with_ceil_test" {
  assert_eq(4, divide_with_ceil(10, 3))
}