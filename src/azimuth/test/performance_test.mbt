// 测试函数和断言函数从 azimuth 包导入

test "basic_add_test" {
  assert_eq(5, add(2, 3))
}

test "basic_multiply_test" {
  assert_eq(6, multiply(2, 3))
}

test "basic_greet_test" {
  assert_eq_string("Hello, World!", greet("World"))
}
